(ns campaign3.curios
  (:require [campaign3.db :as db]
            [randy.core :as r]
            [campaign3.util :as u]))

(def curios #_(db/execute! {:select [:*] :from [:curios]}))
(def inversed-chance 1/3)

(defn new []
  (let [curio (r/sample curios)
        inversed? (u/occurred? inversed-chance)]
    (cond-> curio
            inversed? (-> (assoc :multiplier 0)
                          (update :name #(str "Inversed " %))))))
