(ns campaign3.enchants
  (:require
    (campaign3
      [db :as db]
      [mundanes :as mundanes]
      [prompting :as p]
      [randoms :as randoms]
      [util :as u])
    [clojure.walk :as walk]
    [randy.core :as r]))

(defn prep-matcher [matcher]
  (walk/prewalk
    (fn [x] (if (and (vector? x) (not (map-entry? x)))
              (set x)
              x))
    matcher))

(def enchants (->> (db/load-all :enchants)
                   (map (fn [{:keys [randoms] :as enchant}]
                          (-> enchant
                              (assoc :weighting (randoms/randoms->weighting-multiplier randoms))
                              (update :tags set)
                              (update :randoms randoms/randoms->fn)
                              (update :requires prep-matcher)
                              (update :prohibits prep-matcher))))))

(defn- equality-match? [actual req]
  (when req
    (= req actual)))

(defn- presence-match? [actual req]
  (when (some? req)
    (= req (some? actual))))

(defn- is-allowed? [base base-type prohibits? reqs]
  (reduce
    (fn [_ [kw req]]
      (let [match? (case kw
                     :base-type (equality-match? base-type req)
                     :disadvantaged-stealth (equality-match? (:disadvantaged-stealth base) req)
                     :ranged? (presence-match? (:range base) req)
                     (let [base-value (kw base)]
                       (if (coll? base-value)
                         (some req base-value)
                         (req base-value))))]
        (if (= prohibits? match?)
          (reduced false)
          true)))
    true
    reqs))

(defn- compatible? [base base-type {:keys [requires prohibits]}]
  (and (is-allowed? base base-type true prohibits)
       (is-allowed? base base-type false requires)))

(defn valid-enchants [base base-type]
  (filterv #(compatible? base base-type %) enchants))

(defn ->valid-enchant-fn [valid-enchants]
  (r/alias-method-sampler
    (mapv #(dissoc % :weighting) valid-enchants)
    (mapv :weighting valid-enchants)))

(def ->valid-enchant-fn-memo (memoize (comp ->valid-enchant-fn valid-enchants)))

(defn add-enchants
  ([base type points-target] (add-enchants points-target (->valid-enchant-fn-memo base type)))
  ([points-target enchants-fn]
   (loop [points-sum 0
          enchants []]
     (let [{:keys [points] :as e} (enchants-fn)
           new-points-sum (+ points points-sum)
           new-enchants (conj enchants e)]
       (if (>= new-points-sum points-target)
         (map (comp :effect u/fill-randoms) new-enchants)
         (recur new-points-sum new-enchants))))))

(defn random-enchanted [points-target]
  (let [{:keys [base type]} (mundanes/new)]
    [base (add-enchants base type points-target)]))

(defn >>add []
  (let [{:keys [base type]} (mundanes/>>base)]
    (when (and base type)
      (->> ((->valid-enchant-fn-memo base type))
           (u/fill-randoms)
           (:effect)))))

(defn >>add-totalling []
  (let [points (some-> (p/>>input "Desired points total:") (parse-long))
        {:keys [base type]} (when points (mundanes/>>base))]
    [base
     (add-enchants base type points)]))
