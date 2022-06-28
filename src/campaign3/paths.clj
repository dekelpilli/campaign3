(ns campaign3.paths
  (:require (campaign3
              [db :as db]
              [util :as u])
            [clojure.string :as str]
            [randy.core :as r]))

(def prayer-paths (db/load-all :prayer-paths))


(defn new-divine-dust []
  (cond->> (-> prayer-paths r/sample :name (str/replace #"^Touch of" "Dust of"))
           (u/occurred? 1/5) (str "Refined ")))

(defn- update-progress! [{:keys [character path] :as new-progression}]
  ;TODO
  )

(defn &progress-path! []
  ;TODO
  #_(let [done? #(contains? (:taken %) 10)
        unfinished-paths (filter #(and (not (done? %)) (:enabled? % true)) @prayer-progressions)
        player-paths (group-by :character unfinished-paths)
        path-options (->> player-paths
                          (u/make-options)
                          (u/display-pairs))
        current-progression (->> (u/&num)
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
                                        (u/display-pairs))
            new-latest (u/&num)
            valid (and new-latest (contains? progress-index-options new-latest))]
        (when valid
          (update-progress!
            (update current-progression :taken #(conj % new-latest))))))))
