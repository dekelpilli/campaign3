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

(def ^:private points-per-level 10)

(defn choose-relic [relics]
  (some->> (not-empty relics)
           (u/assoc-by :name)
           (p/>>item "Relic:")))

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

(defn- upgrade-mod [{:keys [committed points upgrade-points effect]
                     :or   {committed 0} :as modifier}
                    points-remaining relic]
  #_(let [upgrade-points (or upgrade-points points 10)
          selected-mod-effect effect]
      (if (>= (+ committed points-remaining) upgrade-points)
        (-> relic
            (update :progressed #(filterv (fn [{:keys [effect]}]
                                            (not= selected-mod-effect effect)) %))
            (update :existing #(mapv (fn [{:keys [effect] :as existing-mod}]
                                       (if (= selected-mod-effect effect)
                                         (-> existing-mod
                                             (update :points (fn [points] (+ upgrade-points points)))
                                             (update :level inc))
                                         existing-mod)) %)))
        (update relic :progressed
                #(as-> % $
                       (filterv (fn [{:keys [effect]}]
                                  (not= selected-mod-effect effect)) $)
                       (conj $ (assoc modifier :committed (+ committed points-remaining))))))))

(defn- attach-new-mod [{:keys [points upgrade-points]
                        :or   {points 10} :as modifier}
                       relic]
  ;new random/player mods are always added as if they have max 10 points
  #_(update relic :existing
            #(conj % (assoc modifier
                       :level 1
                       :points (min points 10)
                       :upgrade-points (or upgrade-points points)))))

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
                         (conj checks new-type))))
                   :upgrade-mod
                   (if (> (:upgrade-mod opts 0) (count upgradeable))
                     (let [types (dissoc types :upgrade-mod)
                           new-type (r/weighted-sample types)]
                       (recur
                         (-> (update opts :upgrade-mod dec)
                             (update new-type (fnil inc 0)))
                         types
                         (conj checks new-type))))
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
                                                          :existing   (map #(assoc % :level 1) start)
                                                          :progressed []})
        relic (assoc relic :attunements character attunement)
        upgradeable (-> (remove (comp false? :upgradeable) existing)
                        seq)
        existing-effects (into #{} (map :effect) existing)
        available-relic-mods (-> (remove (comp existing-effects :effect) mods)
                                 seq)
        gen-random-mod (e/->valid-enchant-fn-memo base base-type)
        levelling-option-types (cond-> {:new-random-mod 1}
                                       available-relic-mods (assoc :new-relic-mod 1)
                                       upgradeable (assoc :upgrade-mod 1))
        options (into (map (fn [progressed] {:type :progress
                                             :mod  progressed}) progressed)
                      (unique-levelling-options
                        (- 2 (count progressed))
                        upgradeable available-relic-mods gen-random-mod levelling-option-types))
        ]
    (when-let [choice (p/>>item "Choose relic levelling option:" (conj options {:type :no-change}))]
      (update-in relic [:attunements character :level] inc)))) ;TODO attach choice results to relic

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
          levelled-relic (reduce (fn [relic _]
                                   (or (single-relic-level relic character base)
                                       (reduced relic)))
                                 relic
                                 (range additional-levels))]
      (update-relic! levelled-relic)
      levelled-relic)))

(defn new! []
  (let [{:keys [base-type] :as relic} (-> (db/execute! {:select [:*]
                                                        :from   [:relics]
                                                        :where  [:= :found false]})
                                          r/sample)]
    (-> relic (select-keys [:name :start :base-type]) puget/cprint)
    (when-let [{:keys [name]} (mundanes/choose-base base-type)]
      (-> relic (assoc :found true :base name) update-relic!))))

(defn change-relic-base! []
  (u/when-let* [{:keys [base-type] :as relic} (choose-found-relic)
                {:keys [name]} (mundanes/choose-base base-type)]
    (-> relic (assoc :base name) update-relic!)))
