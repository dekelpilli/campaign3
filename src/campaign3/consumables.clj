(ns campaign3.consumables
  (:require
    [campaign3
     [util :as util]
     [amounts :as amounts]
     [db :as db]]))

(def consumables (->> (db/execute! {:select [:*] :from [:consumables]})
                      (map #(update % :amount amounts/amount->fn "1d4"))))

(defn new []
  (util/get-rand-amount consumables))
