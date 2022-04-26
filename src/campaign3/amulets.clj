(ns campaign3.amulets
  (:require [randy.core :as r]
            [campaign3.db :as db]
            [campaign3.prompting :as p]))

(def cr-weightings
  {0M     1
   0.125M 2
   0.25M  2
   0.5M   3
   1M     5
   2M     3
   3M     1
   4M     0 5M 0 6M 0 7M 0 8M 0 9M 0 10M 0 11M 0 12M 0 13M 0 14M 0 15M 0 16M 0 17M 0 18M 0 19M 0
   20M    0 21M 0 22M 0 23M 0 24M 0 25M 0 26M 0 27M 0 28M 0 30M 0})

#_(def crs [0M 0.125M 0.25M 0.5M
            1M 2M 3M 4M 5M 6M 7M 8M 9M
            10M 11M 12M 13M 14M 15M 16M 17M 18M 19M
            20M 21M 22M 23M 24M 25M 26M 27M 28M 30M])

(def new-amulet-cr (r/alias-method-sampler cr-weightings))

(defn monsters-by-cr [cr]
  (db/execute! {:select [:*]
                :from   [:monsters]
                :where  [:= :cr cr]}))

(def sample-trait (comp r/sample :traits))

(defn cr->output [cr]
  (let [monster (r/sample (monsters-by-cr cr))]
    {:monster (dissoc monster :traits)
     :trait   (sample-trait monster)}))

(def new (comp cr->output new-amulet-cr))

(defn >>from-cr []
  (some-> ^String (p/>>input "Amulet CR:" (map str (sort (keys cr-weightings)))) ;TODO update when prompting is updated to accept non-stringified coll
          (BigDecimal.)
          (cr->output)))
