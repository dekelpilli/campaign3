(ns campaign3.rings
  (:require
    [campaign3
     [util :as util]
     [db :as db]]
    [clojure.string :as str]))

(def synergy-rings #_(db/execute! {:select [:*]
                                 :from   [:rings]
                                 :where  [:is :synergy true]}))
(def regular-rings #_(db/execute! {:select [:*]
                                 :from   [:rings]
                                 :where  [:is :synergy false]}))
(def all-rings #_(into synergy-rings regular-rings))

(defn new-non-synergy []
  (->> regular-rings
       (util/rand-enabled)
       (util/fill-randoms)))

(defn new-synergy []
  (->> synergy-rings
       (util/rand-enabled)
       (util/fill-randoms)))

(defn &sacrifice []
  (println "Which rings are being sacrificed?")
  (let [ban-opts (as-> all-rings $
                       (map :name $)
                       (util/make-options $ {:sort? true})
                       (util/display-pairs $ {:sort? true :v "Name"}))
        input (read-line)]
    (when-let [sacrificed (->> (str/split input #",")
                               (map #(Integer/parseInt %))
                               (map ban-opts)
                               (filter identity)
                               (not-empty))]
      (let [sacrificed? (set sacrificed)
            num-options (->> sacrificed
                             (filter #(= "The Catalyst" %))
                             (count)
                             (Math/pow 2)
                             (* (count sacrificed)))]
        (->> all-rings
             (remove #(sacrificed? (:name %)))
             (shuffle)
             (take num-options)
             (map util/fill-randoms))))))
