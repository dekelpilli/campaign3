(ns campaign3.prayers
  (:require
    [campaign3
     [db :as db]
     [util :as util]]))

(def prayer-paths #_(db/execute! {:select [:*] :from [:prayer-paths]}))


(defn new-stone []
  (-> prayer-paths util/rand-enabled :name))

(defn- update-progress! [{:keys [character path] :as new-progression}]
  ;TODO
  )

(defn &progress-path! []
  ;TODO
  #_(let [done? #(contains? (:taken %) 10)
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
          (update-progress!
            (update current-progression :taken #(conj % new-latest))))))))
