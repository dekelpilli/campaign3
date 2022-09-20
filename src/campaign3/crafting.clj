(ns campaign3.crafting
  (:require (campaign3
              [amounts :as amounts]
              [db :as db]
              [util :as u])))

(def crafting-items (->> (db/load-all :crafting-items)
                         (map #(update % :amount amounts/amount->fn "1d3"))))

(defn new-crafting-items []
  (u/get-rand-amount crafting-items))
