(ns campaign3.rings
  (:require
    [campaign3
     [util :as u]
     [prompting :as p]
     [db :as db]]
    [randy.core :as r]))

(def all-rings (db/load-all :rings))
(def synergy-rings (filterv :synergy all-rings))
(def regular-rings (filterv (complement :synergy) all-rings))

(defn new-non-synergy []
  (->> regular-rings
       (r/sample)
       (u/fill-randoms)))

(defn new-synergy []
  (->> synergy-rings
       (r/sample)
       (u/fill-randoms)))

(defn >>sacrifice []
  (when-let [sacrificed-rings (->> all-rings
                                   (map :name)
                                   (p/>>checkbox "Which rings are being sacrificed?")
                                   (not-empty))]
    (let [additional-rings (-> (p/>>input "How many non-distinct rings were sacrificed?")
                               (parse-long)
                               (or 0))
          catalysts-used (or (some-> (p/>>input "How many catalysts were used in this ring sacrifice?")
                                     (parse-long))
                             0)
          num-options (-> (count sacrificed-rings)
                          (+ additional-rings)
                          (* (inc catalysts-used))
                          (max (count all-rings)))]
      (->> all-rings
           (r/sample-without-replacement num-options)
           (map u/fill-randoms)))))
