(ns campaign3.uniques
  (:require
    (campaign3
      [db :as db]
      [util :as u])
    [randy.core :as r]))

(def uniques (db/load-all :uniques))

(defn new []
  (-> (r/sample uniques)
      (update :effects #(map (comp :effect u/fill-randoms) %))))
