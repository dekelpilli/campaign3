(ns campaign3.consumables
  (:require (campaign3
              [amounts :as amounts]
              [db :as db]
              [util :as u])))

(def consumables (->> (db/load-all :consumables)
                      (map #(update % :amount amounts/amount->fn "1d4"))))

(defn new []
  (u/get-rand-amount consumables))
