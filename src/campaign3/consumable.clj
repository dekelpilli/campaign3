(ns campaign3.consumable
  (:require
    [campaign3
     [util :as util]
     [db :as db]]))

(def consumables (db/execute! {:select [:*] :from [:consumables]}))

(defn new []
  (util/get-multiple-items consumables #(inc (rand-int 4))))
