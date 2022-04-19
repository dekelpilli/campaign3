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
