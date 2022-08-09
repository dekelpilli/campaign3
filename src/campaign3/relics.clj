(ns campaign3.relics
  (:require (campaign3
              [db :as db]
              [helmets :as helmets]
              [mundanes :as mundanes]
              [prompting :as p]
              [util :as u])
            [puget.printer :as puget]
            [randy.core :as r]
            [campaign3.enchants :as e]))

(defn- choose-found-relic []
  (->> (db/execute! {:select [:*]
                     :from   [:relics]
                     :where  [:= :found true]})
       (u/assoc-by :name)
       (p/>>item "Choose relic:")))

(defn- update-relic! [{:keys [name] :as relic}]
  (db/execute! {:update [:relics]
                :set    (-> relic
                            (select-keys [:attunements :base :found])
                            (update :attunements u/jsonb-lift))
                :where  [:= :name name]}))

(defn- upgrade-mod [attunement {:keys [effect upgrade-points] :as mod}]
  (if (> upgrade-points 10)
    (update attunement :progressed conj (assoc mod :committed 10))
    (update attunement :existing
            #(map (fn [{existing-effect :effect :as existing-mod}]
                    (if (= existing-effect effect)
                      (update mod :level + (-> (/ upgrade-points 10) int))
                      existing-mod))
                  %))))

(defn- progress-mod [attunement {:keys [committed upgrade-points effect] :as mod}]
  (let [committed (+ committed 10)]
    (if (>= committed upgrade-points)
      (-> attunement
          (update :progressed #(remove (comp #{effect} :effect) %))
          (update :existing #(map (fn [{existing-effect :effect :as existing-mod}]
                                    (if (= existing-effect effect)
                                      (update mod :level inc)
                                      existing-mod))
                                  %)))
      (-> attunement
          (update :progressed #(map (fn [{progressed-effect :effect :as progressed-mod}]
                                      (cond-> progressed-mod
                                              (= progressed-effect effect) (assoc :committed committed)))
                                    %))))))

(defn- prep-new-mod [{:keys [points upgrade-points]
                      :or   {points 10}
                      :as   mod}]
  (assoc mod
    :level 1
    :points (min points 10)
    :upgrade-points (or upgrade-points points 10)))

(defn- attach-new-mod [attunement mod]
  (update attunement :existing conj (prep-new-mod mod)))

(defn add-mod-choice [attunement {:keys [type mod]}]
  (case type
    (:new-random-mod :new-relic-mod) (attach-new-mod attunement mod)
    :progress (progress-mod attunement mod)
    :upgrade-mod (upgrade-mod attunement mod)
    :none attunement))

(defn unique-levelling-options [n upgradeable relic-mods random-gen option-types]
  (if (zero? n)
    []
    (let [opts (loop [opts (-> (repeatedly n #(r/weighted-sample option-types)) frequencies)
                      types option-types
                      [check-kw & checks] [:new-relic-mod :upgrade-mod]]
                 (case check-kw
                   :new-relic-mod
                   (if (> (:new-relic-mod opts 0) (count relic-mods))
                     (let [types (dissoc types :new-relic-mod)
                           new-type (r/weighted-sample types)]
                       (recur
                         (-> (update opts :new-relic-mod dec)
                             (update new-type (fnil inc 0)))
                         types
                         (conj checks new-type)))
                     (recur opts types checks))
                   :upgrade-mod
                   (if (> (:upgrade-mod opts 0) (count upgradeable))
                     (let [types (dissoc types :upgrade-mod)
                           new-type (r/weighted-sample types)]
                       (recur
                         (-> (update opts :upgrade-mod dec)
                             (update new-type (fnil inc 0)))
                         types
                         (conj checks new-type)))
                     (recur opts types checks))
                   :new-random-mod (recur opts types checks)
                   nil opts))]
      (mapcat (fn [[type amount]]
                (let [mods (case type
                             :new-relic-mod (r/sample-without-replacement amount relic-mods)
                             :upgrade-mod (r/sample-without-replacement amount upgradeable)
                             :new-random-mod (repeatedly amount random-gen))]
                  (map (fn [mod] {:type type :mod mod}) mods)))
              opts))))

(defn single-relic-level [{:keys [attunements base-type start mods] :as relic} character base]
  (let [{:keys [existing progressed] :as attunement} (or (character attunements)
                                                         {:level      1
                                                          :existing   (map prep-new-mod start)
                                                          :progressed []})
        relic (assoc-in relic [:attunements character] attunement)
        upgradeable (-> (remove (comp false? :upgradeable) existing)
                        seq)
        existing-effects (into #{} (map :effect) existing)
        available-relic-mods (-> (remove (comp existing-effects :effect) mods)
                                 seq)
        gen-random-mod (comp u/fill-randoms (e/->valid-enchant-fn-memo base base-type))
        levelling-option-types (cond-> {:new-random-mod 1}
                                       available-relic-mods (assoc :new-relic-mod 1)
                                       upgradeable (assoc :upgrade-mod 1))
        options (into (map (fn [progressed] {:type :progress
                                             :mod  progressed}) progressed)
                      (unique-levelling-options
                        (- 2 (count progressed))
                        upgradeable available-relic-mods gen-random-mod levelling-option-types))]
    (when-let [choice (p/>>item "Choose relic levelling option:" (conj options {:type :no-change}) :sorted? false)]
      (-> relic
          (update-in [:attunements character :level] inc)
          (update-in [:attunements character] add-mod-choice choice)))))

(defn level-relic! []
  (u/when-let* [{:keys [attunements base-type base] :as relic} (choose-found-relic)
                character (->> (keys helmets/character-enchants)
                               (p/>>item "Character:")
                               keyword)
                additional-levels (if (character attunements)
                                    1
                                    (some-> (p/>>item (format "What is %s's %s relic level?" (name character) base-type)
                                                      (range 2 11))
                                            dec))]
    (let [base (mundanes/name->base base-type base)
          {:keys [attunements] :as levelled-relic} (reduce (fn [relic _]
                                                             (or (single-relic-level relic character base)
                                                                 (reduced relic)))
                                                           relic
                                                           (range additional-levels))]
      (update-relic! levelled-relic)
      (character attunements))))

(defn find-relic! [{:keys [base-type] :as relic}]
  (when-let [{:keys [name]} (mundanes/choose-base base-type)]
    (-> relic (assoc :found true :base name) update-relic!)))

(defn new! []
  (let [relic (-> (db/execute! {:select [:*]
                                :from   [:relics]
                                :where  [:= :found false]})
                  r/sample)]
    (-> relic (select-keys [:name :start :base-type]) puget/cprint)
    (find-relic! relic)))

(defn reveal-relic! []
  (when-let [relic (->> (db/execute! {:select [:*]
                                      :from   [:relics]
                                      :where  [:= :found false]})
                        (u/assoc-by :name)
                        (p/>>item "Relic:"))]
    (-> relic (select-keys [:name :start :base-type]) puget/cprint)
    (find-relic! relic)))

(defn change-relic-base! []
  (u/when-let* [{:keys [base-type] :as relic} (choose-found-relic)
                {:keys [name]} (mundanes/choose-base base-type)]
    (-> relic (assoc :base name) update-relic!)))
