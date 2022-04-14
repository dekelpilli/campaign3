(ns campaign3.uniques
  (:require
    [campaign3
     [util :as util]
     [db :as db]]
    [clojure.walk :as walk]))

(def uniques (db/execute! {:select [:*] :from [:uniques]}))

(defn new []
  (let [{:keys [effects] :as unique} (rand-nth uniques)]
    (loop [[current & remaining] effects
           unique unique
           n 1]
      (if current
        (recur remaining
               (assoc unique (str n) (:effect (util/fill-randoms current)))
               (inc n))
        (as-> unique $
              (dissoc $ :effects)
              (walk/stringify-keys $)
              (into (sorted-map) $))))))
