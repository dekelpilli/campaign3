(ns campaign3.prayer
  (:require
    [campaign3
     [state :refer [prayer-paths prayer-progressions override-prayer-progress!]]
     [util :as util]]))

(defn new-stone []
  (-> @prayer-paths util/rand-enabled :name))

(defn- override-progress! [{:keys [character path] :as new-progression}]
  (override-prayer-progress! (mapv
                               #(if (and (= (:character %) character) (= (:path %) path)) new-progression %)
                               @prayer-progressions)))

(defn &progress-path! []
  (let [done? #(contains? (:taken %) 10)
        unfinished-paths (filter #(and (not (done? %)) (:enabled? % true)) @prayer-progressions)
        player-paths (group-by :character unfinished-paths)
        path-options (->> player-paths
                          (util/make-options)
                          (util/display-pairs))
        current-progression (->> (util/&num)
                                 (path-options)
                                 (player-paths)
                                 (first))
        prayer-path (->> @prayer-paths
                         (filter #(= (:path current-progression) (:name %)))
                         (first))]
    (when prayer-path
      (let [progress-index-options (->> (range (reduce max (current-progression :taken)) 10)
                                        (take 2)
                                        (map (fn [i] [i (nth (prayer-path :levels) i)]))
                                        (map (fn [[k v]] [(inc k) v]))
                                        (into {})
                                        (util/display-pairs))
            new-latest (util/&num)
            valid (and new-latest (contains? progress-index-options new-latest))]
        (when valid
          (override-progress!
            (update current-progression :taken #(conj % new-latest))))))))
