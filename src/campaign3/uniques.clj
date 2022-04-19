(ns campaign3.uniques
  (:require
    [campaign3
     [util :as u]
     [db :as db]]
    [randy.core :as r]))

(def uniques (db/execute! {:select [:*] :from [:uniques]}))

(defn new []
  (-> (r/sample uniques)
      (update :effects #(map (comp :effect u/fill-randoms) %))))
