(ns campaign3.util
  (:require [table.core :as t]))

(defn jsonb-lift [x]
  (when x [:lift x]))

(defn- table [out]
  (binding [table.width/*width* (delay 9999)]
    (t/table out :style :unicode-3d)))

(defn display-multi-value [coll]
  (table (if (sequential? coll) coll [coll]))
  coll)

(defn display-pairs
  ([m] (display-pairs m nil))
  ([m {:keys [sort? k v]
       :or   {sort? false
              k     "Key"
              v     "Value"}}]
   (table
     (as-> m $
           (into [] $)
           (if sort? (sort $) $)
           (concat [[k v]] $)))
   m))

(defn make-options
  ([coll] (make-options coll nil))
  ([coll {:keys [sort?] :or {sort? false}}]
   (as-> (if (map? coll) (keys coll) coll) $
         (if sort? (sort $) $)
         (map-indexed (fn [i option] [i option]) $)
         (into {} $))))

(defn rand-enabled [coll]
  (as-> coll $
        (remove (comp false? :enabled?) $)
        (if (empty? $) nil (rand-nth $))
        (when $ (dissoc $ :enabled?))))

(defn fill-randoms [{:keys [randoms] :as item-modifier}]
  (if (seq randoms)
    (-> item-modifier
        (update :effect #(apply format % (map rand-nth randoms)))
        (dissoc :randoms))
    item-modifier)) ;TODO remove in favour of randoms syntax

(defn occurred? [likelihood-probability]
  (< (rand) likelihood-probability))

(defn get-rand-amount [coll]
  (-> (rand-nth coll)
      (update :amount #(%))
      (fill-randoms))) ;TODO move to amounts ns(?)

(defn assoc-by [f coll]
  (into {} (map (juxt f identity)) coll))
