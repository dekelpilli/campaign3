(ns campaign3.relics
  (:require (campaign3
              [db :as db]
              [enchants :as e]
              [mundanes :as mundanes]
              [prompting :as p]
              [util :as u])
            [puget.printer :as puget]
            [randy.core :as r]))

(defn- choose-found-relic []
  (->> (db/execute! {:select [:*]
                     :from   [:relics]
                     :where  [:= :found true]})
       (u/assoc-by :name)
       (p/>>item "Choose relic:")))

(defn- update-relic! [{:keys [name] :as relic}]
  (db/execute! {:update [:relics]
                :set    (-> relic
                            (select-keys [:levels :base :found])
                            (update :levels u/jsonb-lift))
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

(defn- progress-mod [level {:keys [committed upgrade-points effect] :as mod}]
  (let [committed (+ committed 10)]
    (if (>= committed upgrade-points)
      (-> level
          (update :progressed #(remove (comp #{effect} :effect) %))
          (update :existing #(map (fn [{existing-effect :effect :as existing-mod}]
                                    (if (= existing-effect effect)
                                      (update mod :level inc)
                                      existing-mod))
                                  %)))
      (update level :progressed #(map (fn [{progressed-effect :effect :as progressed-mod}]
                                        (cond-> progressed-mod
                                                (= progressed-effect effect) (assoc :committed committed)))
                                      %)))))

(defn- prep-new-mod [{:keys [points upgrade-points]
                      :or   {points 10}
                      :as   mod}]
  (assoc mod
    :level 1
    :points (min points 10)
    :upgrade-points (or upgrade-points points 10)))

(defn- attach-new-mod [attunement mod]
  (update attunement :existing conj (prep-new-mod mod)))

(defn- add-mod-choice [attunement {:keys [type mod]}]
  (case type
    (:new-random-mod :new-relic-mod) (attach-new-mod attunement mod)
    :progress (progress-mod attunement mod)
    :upgrade-mod (upgrade-mod attunement mod)
    :none attunement))

(defn- unique-levelling-options [n upgradeable relic-mods random-gen option-types]
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

(defn- single-relic-level [{:keys [base-type mods]}
                          {:keys [existing progressed] :as previous-level} base]
  (let [upgradeable (-> (remove (comp false? :upgradeable) existing)
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
      (-> previous-level
          (add-mod-choice choice)
          (update :level inc)))))

(defn set-relic-level! []
  (u/when-let* [{:keys [levels base-type base start] :as relic} (choose-found-relic)
                target-level (p/>>item "What is the relic's new level?" (range 2 11))]
    (let [levels (or (not-empty levels)
                     [{:level      1
                       :existing   (map prep-new-mod start)
                       :progressed []}])
          current-max-level (-> levels peek :level)
          additional-levels (- target-level current-max-level)]
      (if (pos? additional-levels)
        (let [base (mundanes/name->base base-type base)
              new-levels (reduce (fn [levels _]
                                   (if-let [new-level (single-relic-level relic (peek levels) base)]
                                     (conj levels new-level)
                                     (reduced levels)))
                                 levels
                                 (range additional-levels))]
          (update-relic! (assoc relic :levels new-levels))
          (peek new-levels))
        (nth levels (dec target-level))))))

(defn reset-relic! []
  (when-let [relic (choose-found-relic)]
    (update-relic!
      (update relic :levels (comp u/jsonb-lift vector first)))))

(defn find-relic! [{:keys [base-type] :as relic}]
  (when-let [{:keys [name]} (mundanes/choose-base base-type)]
    (-> relic (assoc :found true :base name) update-relic!)))

(defn new-relic! []
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
