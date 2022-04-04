(ns campaign3.ring
  (:require
    [campaign3
     [util :as util]
     [db :as db]]
    [clojure.string :as str]))

(def synergy-rings (db/execute! {:select [:*]
                                 :from   [:rings]
                                 :where  [:is :synergy true]}))
(def regular-rings (db/execute! {:select [:*]
                                 :from   [:rings]
                                 :where  [:is :synergy false]}))

(defn- synergy? [{:keys [name]}]
  (str/starts-with? name "The"))

(defn new-non-synergy []
  (->> synergy-rings
       (remove synergy?)
       (util/rand-enabled)
       (util/fill-randoms)))

(defn new-synergy []
  (->> regular-rings
       (filter synergy?)
       (util/rand-enabled)
       (util/fill-randoms)))

(defn &sacrifice []
  (println "Which rings are being sacrificed?")
  (let [rings (into synergy-rings regular-rings)
        ban-opts (as-> rings $
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
        (->> rings
             (remove #(sacrificed? (:name %)))
             (shuffle)
             (take num-options)
             (map util/fill-randoms))))))
