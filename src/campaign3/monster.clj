(ns campaign3.monster
  (:require [campaign3
             [state :refer [monsters]]]
            [campaign3.util :as util]))

(defn of-cr [cr]
  (when-let [cr-monsters (@monsters cr)]
    (when-not (empty? cr-monsters) (rand-nth cr-monsters))))

(defn &new []
  (loop []
    (let [cr (util/&num)
          mon (of-cr cr)]
      (when (and cr (not (neg? cr)))
        (util/display-pairs mon)
        (recur)))))

(defn generate-amulet []
  (let [weighted-crs {0   5
                      1/8 5
                      1/4 5
                      1/2 10
                      1   25
                      2   25
                      3   15
                      4   10}
        cr (util/weighted-rand-choice weighted-crs)]
    (assoc (of-cr cr) :cr cr)))
