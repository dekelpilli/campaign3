(ns campaign3.tarot
  (:require [campaign3.db :as db]
            [campaign3.prompting :as p]
            [campaign3.util :as u]))

(def cards (->> (db/load-all :tarot-cards)
                (u/assoc-by :name)))

(defn lookup []
  (-> (p/>>item "What is the Tarot card?" cards)
      :effect))

;TODO generate enchants with tags
