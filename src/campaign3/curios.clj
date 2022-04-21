(ns campaign3.curios
  (:require [campaign3.db :as db]
            [randy.core :as r]
            [campaign3.util :as u]))

(def curios (db/load-all :curios))

(defn new []
  (let [curio (r/sample curios)
        inversed? (u/occurred? 1/3)]
    (cond-> curio
            inversed? (-> (assoc :multiplier 0)
                          (update :name #(str "Inversed " %))))))
