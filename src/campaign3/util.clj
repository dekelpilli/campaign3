(ns campaign3.util
  (:require [randy.core :as r]
            [randy.rng :as rng]))

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
  (< (rng/next-double r/default-rng) likelihood-probability))

(defn get-rand-amount [coll]
  (-> (r/sample coll)
      (update :amount #(%))
      (fill-randoms)))

(defn assoc-by [f coll]
  (into {} (map (juxt f identity)) coll))
