(ns campaign3.helmets
  (:require [campaign3.db :as db]
            [campaign3.prompting :as p]
            [randy.core :as r]
            [campaign3.enchants :as e]))

(def character-enchants (->> (db/load-all :character-enchants)
                             (group-by :character)))

(defn new []
  (when-let [enchants (p/>>item "Character name:" character-enchants)]
    (e/add-enchants 20 #(r/sample enchants))))
