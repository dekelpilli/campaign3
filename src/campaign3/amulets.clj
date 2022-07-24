(ns campaign3.amulets
  (:require [campaign3.db :as db]
            [campaign3.prompting :as p]
            [randy.core :as r]))

(def cr-weightings
  {0M     5
   0.125M 10
   0.25M  10
   0.5M   20
   1M     45
   2M     30
   3M     10
   4M     5
   5M     0 6M 0 7M 0 8M 0 9M 0 10M 0 11M 0 12M 0 13M 0 14M 0 15M 0 16M 0 17M 0 18M 0 19M 0 20M 0 21M 0 22M 0 23M 0 24M 0 25M 0 26M 0 27M 0 28M 0 30M 0})

(def new-amulet-cr (r/alias-method-sampler cr-weightings))

(defn- ->output [monster trait]
  (-> monster
      (dissoc :traits)
      (assoc :trait trait)))

(defn monster-traits-by-cr [cr]
  (let [monsters (db/execute! {:select [:*]
                               :from   [:monsters]
                               :where  [:= :cr cr]})]
    (reduce
      (fn [acc {:keys [traits] :as monster}]
        (into acc (map #(->output monster %)) traits))
      []
      monsters)))

(def cr->output (comp r/sample monster-traits-by-cr))
(def new (comp cr->output new-amulet-cr))

(defn- cr []
  (p/>>input "Amulet CR:" (keys cr-weightings)))

(defn sample-n []
  (when-let [cr (cr)]
    (when-let [amount (some-> (p/>>input "Amount of monster traits:") (parse-long))]
      (let [monster-traits (monster-traits-by-cr cr)]
        (cond->> monster-traits
                 (> (count monster-traits) amount) (r/sample-without-replacement amount))))))

(defn from-cr []
  (some-> (cr) cr->output))

;TODO get by CR+monster type
