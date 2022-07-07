(ns campaign3.relics
  (:require (campaign3
              [mundanes :as mundanes]
              [prompting :as p]
              [util :as u])))

(def ^:private points-per-level 10)

(def upgrade-prices [0 100 200 300 400 500 600 700 800 1500])

(defn choose-relic [relics]
  (some->> (not-empty relics)
           (u/assoc-by :name)
           (p/>>item "Relic:")))

(defn- &owned []
  #_(let [owned? (fn [{:keys [found? enabled?]
                       :or   {enabled? true}}] (and found? enabled?))]
      (->> @relics
           (filter owned?)
           (choose-relic))))

(defn- upgradeable []
  #_(let [upgradeable? (fn [{:keys [found? enabled? level]
                             :or   {enabled? true}}] (and found? enabled? (<= level 10)))]
      (->> @relics
           (filter upgradeable?)
           (choose-relic))))

(defn- update-relic! [{:keys [name] :as relic}]
  )

(defn- upgrade-mod [{:keys [committed points upgrade-points effect]
                     :or   {committed 0} :as modifier}
                    points-remaining relic]
  (let [upgrade-points (or upgrade-points points 10)
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
  (update relic :existing
          #(conj % (assoc modifier
                     :level 1
                     :points (min points 10)
                     :upgrade-points (or upgrade-points points)))))

(defn level-relic! ;TODO relic levelling process needs to be saved per-character
  ([] (when-let [relic (upgradeable)]
        (level-relic! relic)))
  ([{:keys [level existing base type available progressed owner] :as relic}]
   #_(let [points-remaining (- (* points-per-level (inc level))
                               (->> existing
                                    (map #(:points % 10))
                                    (reduce + 0))
                               (->> progressed
                                    (map :committed)
                                    (reduce +)))
           upgradeable-mods (filter #(:upgradeable? % true) existing)
           possible-options (cond-> [:new-relic-mod :new-random-mod :new-character-mod]
                                    (seq upgradeable-mods) (conj :upgrade-existing-mod))
           upgrade-options (concat [:none]
                                   (repeat (count progressed) :continue-progress)
                                   (repeatedly #(r/sample possible-options)))
           valid-enchants (e/find-valid-enchants base type)
           rand-filled #(->> % util/rand-enabled util/fill-randoms)
           mod-options (->> upgrade-options
                            (map (fn [o]
                                   [o (rand-filled
                                        (case o
                                          :continue-progress progressed
                                          :none [nil]
                                          :new-character-mod (@character-enchants owner)
                                          :new-relic-mod available
                                          :upgrade-existing-mod upgradeable-mods
                                          :new-random-mod valid-enchants))]))
                            (dedupe)
                            (take 3)
                            (map-indexed #(into [%1] %2))
                            (into [["Key" "Type" "Value"]])
                            (util/display-multi-value))
           choice (util/&num)
           [_ option-type modifier] (when (and choice (>= choice 0)) (nth mod-options (inc choice)))]
       (when option-type
         (-> (case option-type
               (:new-character-mod :new-random-mod) (attach-new-mod modifier relic)
               (:continue-progress :upgrade-existing-mod) (upgrade-mod modifier points-remaining relic)
               :new-relic-mod (-> relic
                                  (update :available #(filterv (fn [m] (not= modifier m)) %))
                                  (update :existing #(conj % modifier)))
               :none relic)
             (update :level inc)
             (update-relic!))))))

(defn new! []
  #_(let [relic (->> @relics
                     (remove :found?)
                     (util/rand-enabled))]
      (if relic
        (util/display-multi-value (dissoc relic :available :found? :level))
        (throw (Exception. "Out of relics :(")))
      (let [base (mundanes/choose-base (:type relic))
            owner (when base (util/&choose (keys @character-enchants)))]
        (when (and base owner)
          (update-relic! (assoc relic :found? true
                                      :base base
                                      :owner owner))))))

(defn sell! []
  )
