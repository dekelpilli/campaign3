(ns campaign3.crafting
  (:require
    [campaign3
     [util :as u]
     [db :as db]
     [amounts :as amounts]]))

(def crafting-items (->> (db/execute! {:select [:*] :from [:crafting-items]})
                         (map #(update % :amount amounts/amount->fn "1d3"))))

(defn new []
  (u/get-rand-amount crafting-items))
