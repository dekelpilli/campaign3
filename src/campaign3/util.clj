(ns campaign3.util
  (:require [table.core :as t]
            [randy.core :as r]))

(defn jsonb-lift [x]
  (when x [:lift x]))

(defn rand-enabled [coll]
  (as-> coll $
        (remove (comp false? :enabled?) $)
        (if (empty? $) nil (r/sample $))
        (when $ (dissoc $ :enabled?))))

(defn fill-randoms [{:keys [randoms] :as item-modifier}]
  (cond-> (dissoc item-modifier :randoms)
          randoms (update :effect #(apply format % (randoms)))))

(defn occurred? [likelihood-probability]
  (< (rand) likelihood-probability))

(defn get-rand-amount [coll]
  (-> (r/sample coll)
      (update :amount #(%))
      (fill-randoms))) ;TODO move to amounts ns(?)

(defn assoc-by [f coll]
  (into {} (map (juxt f identity)) coll))
