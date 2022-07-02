(ns campaign3.helmets
  (:require [campaign3.db :as db]
            [campaign3.prompting :as p]
            [randy.core :as r]
            [campaign3.enchants :as e]
            [campaign3.util :as u]))

(def character-enchants (->> (db/load-all :character-enchants)
                             (group-by :character)))

(defn new []
  (when-let [enchants (p/>>item "Character name:" character-enchants)]
    (e/add-enchants 20 #(r/sample enchants))))

(defn upgrade []
  (when-let [available-enchants (p/>>item "Character name:" character-enchants)]
    (when-let [present-enchants (seq (p/>>distinct-items "Present enchants:" available-enchants))]
      (when-let [enchant-levels (into {}
                                      #(parse-long (p/>>input (str "What is the level of '" (:effect %) "'")))
                                      present-enchants)]
        (let [upgraded-enchant (r/sample present-enchants)
              points-total (reduce-kv (fn [total level {:keys [points]}] (+ total (* level points)))
                                      10 enchant-levels)
              fractured? (u/occurred? (/ 10 points-total))]
          {:upgraded   upgraded-enchant
           :fractured? fractured?})))))
