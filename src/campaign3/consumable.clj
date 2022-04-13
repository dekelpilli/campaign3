(ns campaign3.consumable
  (:require
    [campaign3
     [util :as util]
     [dsls :as dsls]
     [db :as db]]))

(def consumables (->> (db/execute! {:select [:*] :from [:consumables]})
                      (map #(dsls/roll->fn % "1d4"))))

(defn new []
  (util/get-rand-rollable consumables))
