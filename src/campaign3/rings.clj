(ns campaign3.rings
  (:require
    [campaign3
     [util :as u]
     [prompting :as p]
     [randoms :as randoms]
     [db :as db]]
    [randy.core :as r]
    [clojure.string :as str]))

(def synergy-rings (->> (db/execute! {:select [:*]
                                      :from   [:rings]
                                      :where  [:is :synergy true]})
                        (map #(update % :randoms randoms/randoms->fn))))
(def regular-rings (->> (db/execute! {:select [:*]
                                      :from   [:rings]
                                      :where  [:is :synergy false]})
                        (map #(update % :randoms randoms/randoms->fn))))
(def all-rings (into synergy-rings regular-rings))

(defn new-non-synergy []
  (->> regular-rings
       (r/sample)
       (u/fill-randoms)))

(defn new-synergy []
  (->> synergy-rings
       (r/sample)
       (u/fill-randoms)))

(defn &sacrifice []
  (println "Which rings are being sacrificed?")
  (let [ban-opts (as-> all-rings $
                       (map :name $)
                       (u/make-options $ {:sort? true})
                       (u/display-pairs $ {:sort? true :v "Name"}))
        input (read-line) ;TODO replace
        catalysts-used (or (some-> (p/>>input "How many catalysts were used in this ring sacrifice?")
                                   (parse-long))
                           0)]
    (when-let [sacrificed (->> (str/split input #",")
                               (map #(Integer/parseInt %))
                               (map ban-opts)
                               (filter identity)
                               (not-empty))]
      (let [sacrificed? (set sacrificed)
            num-options (* (count sacrificed) (inc catalysts-used))]
        (->> all-rings
             (remove #(sacrificed? (:name %)))
             (shuffle)
             (take num-options)
             (map u/fill-randoms))))))
