(ns campaign3.util
  (:require [table.core :as t]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #(<= % r) w)))))

(defn ->num [s]
  (try
    (let [n (edn/read-string s)]
      (when (number? n) n))
    (catch Exception _)))

(defn &num []
  (->num (read-line)))

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

(defn &choose
  ([coll] (&choose coll nil))
  ([coll opts]
   (let [options (display-pairs (make-options coll opts) opts)]
     (when-let [n (&num)]
       (as-> (options n) $
             (if (map? coll) (coll $) $))))))

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
    item-modifier))

(defn occurred? [likelihood-probability]
  (< (rand) likelihood-probability))

(defn- disadv [f] #(min (f) (f)))
(defn- adv [f] #(max (f) (f)))
(defn- multi [f multiplier-str] #(* (f) (Long/parseLong (subs multiplier-str 1))))
(defn- static [const] (constantly (Long/parseLong const)))
(defn get-multiple-items [coll f]
  (let [{:keys [metadata] :as item} (rand-enabled coll)
        randomiser (reduce #(cond
                              (= "disadvantage" %2) (disadv %1)
                              (= "advantage" %2) (adv %1)
                              (str/starts-with? %2 "x") (multi %1 %2)
                              :else (static %2))
                           f metadata)]
    (-> item
        (assoc :amount (randomiser))
        (fill-randoms)
        (dissoc :metadata))))

;d100 + this for a positive random encounter, with result being the total points of a magic item?
; could also give nothing if they go above the number, or a low loot roll
(defn cheiro-sum [word maximum]
  (let [char-values {\a 1 \b 2 \c 3 \d 4 \e 5
                     \f 8 \g 3 \h 5 \i 1 \j 1
                     \k 2 \l 3 \m 4 \n 5 \o 7
                     \p 8 \q 1 \r 2 \s 3 \t 4
                     \u 6 \v 6 \w 6 \x 5 \y 1 \z 7}
        sum (transduce (map char-values) + 0 (str/lower-case word))]
    (loop [sum sum]
      (if (or (<= sum maximum) (< sum 10))
        sum
        (recur (transduce (map #(- (int %) 48)) + 0 (str sum)))))))
