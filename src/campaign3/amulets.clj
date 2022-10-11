(ns campaign3.amulets
  (:require [campaign3.db :as db]
            [campaign3.prompting :as p]
            [campaign3.util :as u]
            [randy.core :as r]))

(def ^:private cr-weightings
  {0M 0 0.125M 0 0.25M 0 0.5M 0
   1M 45
   2M 30
   3M 10
   4M 5
   5M 0 6M 0 7M 0 8M 0 9M 0 10M 0 11M 0 12M 0 13M 0 14M 0 15M 0 16M 0 17M 0 18M 0 19M 0 20M 0 21M 0 22M 0 23M 0 24M 0 25M 0 26M 0 27M 0 28M 0 30M 0})

(def ^:private new-amulet-cr (r/alias-method-sampler cr-weightings))

(defn- ->output [monster trait]
  (-> monster
      (dissoc :traits)
      (assoc :trait trait)))

(defn- monsters->traits [monsters]
  (reduce
    (fn [acc {:keys [traits] :as monster}]
      (into acc (map #(->output monster %)) traits))
    []
    monsters))

(defn- monster-traits-by-cr [cr]
  (-> (db/execute! {:select [:*]
                    :from   [:monsters]
                    :where  [:= :cr cr]})
      monsters->traits))

(def ^:private cr->output (comp r/sample monster-traits-by-cr))
(def new-amulet (comp cr->output new-amulet-cr))

(defn- cr []
  (p/>>input "Amulet CR:" (keys cr-weightings)))

(defn sample-amulets []
  (u/when-let* [cr (cr)
                amount (some-> (p/>>input "Amount of monster traits:") (parse-long))]
    (let [monster-traits (monster-traits-by-cr cr)]
      (cond->> monster-traits
               (> (count monster-traits) amount) (r/sample-without-replacement amount)))))

(defn cr->amulet []
  (some-> (cr) cr->output))

(defn amulet-by-monster-type []
  (u/when-let* [cr (cr)
                monsters (->> (db/execute! {:select [:*]
                                            :from   [:monsters]
                                            :where  [:= :cr cr]})
                              (group-by :type)
                              (p/>>item "Choose monster type:"))]
    (-> monsters monsters->traits r/sample)))
