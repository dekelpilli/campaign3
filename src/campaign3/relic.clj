(ns campaign3.relic
  (:require [campaign3
             [state :refer [relics override-relics! character-enchants]]
             [util :as util]
             [enchant :as e]
             [mundane :as mundane]]))

(def ^:private points-per-level 10)

(def upgrade-prices [0 75 200 300 600 800 1100 1300 1500 2000])

(defn &choose-relic [relics]
  (some->> relics
           (not-empty)
           (map (fn [relic] [(:name relic) relic]))
           (into {})
           (util/&choose)))

(defn- &owned []
  (let [owned? (fn [{:keys [found? enabled?]
                     :or   {enabled? true}}] (and found? enabled?))]
    (->> @relics
         (filter owned?)
         (&choose-relic))))

(defn- &upgradeable []
  (let [upgradeable? (fn [{:keys [found? enabled? level]
                           :or   {enabled? true}}] (and found? enabled? (<= level 10)))]
    (->> @relics
         (filter upgradeable?)
         (&choose-relic))))

(defn- override-relic! [{:keys [name] :as relic}]
  (override-relics! (mapv #(if (= (:name %) name) relic %) @relics)))

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

(defn &level-relic!
  ([] (when-let [relic (&upgradeable)]
        (&level-relic! relic)))
  ([{:keys [level existing base type available progressed owner] :as relic}]
   (let [points-remaining (- (* points-per-level (inc level))
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
                                 (repeatedly #(rand-nth possible-options)))
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
           (override-relic!))))))

(defn &new! []
  (let [relic (->> @relics
                   (remove :found?)
                   (util/rand-enabled))]
    (if relic
      (util/display-multi-value (dissoc relic :available :found? :level))
      (throw (Exception. "Out of relics :(")))
    (let [base (mundane/&base (:type relic))
          owner (when base (util/&choose (keys @character-enchants)))]
      (when (and base owner)
        (override-relic! (assoc relic :found? true
                                      :base base
                                      :owner owner))))))

(defn &sell! []
  (when-let [{:keys [name level] :as relic} (&owned)]
    (println "Sell" name "for" (int (+ 300 (/ (reduce + (take level upgrade-prices)) 2))) "?")
    (when (util/&choose [true false])
      (override-relic! (assoc relic :enabled? false)))))
