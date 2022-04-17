(ns campaign3.enchants
  (:require
    [campaign3
     [db :as db]
     [util :as u]
     [mundanes :as mundane]
     [randoms :as randoms]
     [prompting :as p]]
    [randy.core :as r]
    [clojure.walk :as walk]))

(defn prep-matcher [matcher]
  (walk/prewalk
    (fn [x] (if (and (vector? x) (not (map-entry? x)))
              (set x)
              x))
    matcher))

(def enchants (->> (db/execute! {:select [:*] :from [:enchants]})
                   (map (fn [e]
                          (-> e
                              (update :randoms randoms/randoms->fn)
                              (update :requires prep-matcher)
                              (update :prohibits prep-matcher))))))

(defn- equality-match? [actual req]
  (when req
    (= req actual)))

(defn- presence-match? [actual req]
  (when (some? req)
    (= req (some? actual))))

(defn prohibits? [{:keys [range] :as base}
                  given-base-type
                  {:keys [base-type ranged?] :as prohibits}]
  (or (false? (equality-match? given-base-type base-type))
      (false? (equality-match? (:disadvantaged-stealth base) (:disadvantaged-stealth prohibits)))
      (false? (presence-match? range ranged?))
      (reduce
        (fn [_ [kw req]]
          (let [base-value (kw base)]
            (if (if (coll? base-value)
                  (some req base-value)
                  (req base-value))
              (reduced true)
              false)))
        false
        (dissoc prohibits :base-type :ranged? :disadvantaged-stealth))))

(defn meets-requirements? [{:keys [range] :as base}
                           given-base-type
                           {:keys [base-type ranged?] :as requires}]
  (and (not (false? (equality-match? given-base-type base-type)))
       (not (false? (equality-match? (:disadvantaged-stealth base) (:disadvantaged-stealth requires))))
       (not (false? (presence-match? range ranged?)))
       (reduce
         (fn [_ [kw req]]
           (let [base-value (kw base)]
             (if (cond
                   (coll? base-value) (some req base-value)
                   (req base-value))
               false
               (reduced true))))
         true
         (dissoc requires :base-type :ranged? :disadvantaged-stealth))))

(defn- compatible? [base base-type {:keys [requires prohibits]}]
  (and (not (prohibits? base base-type prohibits))
       (meets-requirements? base base-type requires)))

(defn find-valid-enchants [base base-type]
  (filterv #(compatible? base base-type %) enchants))

(defn find-valid-enchants-memo (memoize find-valid-enchants))

(defn add-enchants [base type points-target]
  (let [valid-enchants (find-valid-enchants-memo base type)]
    (loop [points-sum 0
           enchants []]
      (let [{:keys [points] :as e} (r/sample valid-enchants)
            new-points-sum (+ points points-sum)
            new-enchants (conj enchants e)]
        (if (> new-points-sum points-target)
          (map (comp u/prep-map u/fill-randoms) new-enchants)
          (recur new-points-sum new-enchants))))))

(defn random-enchanted [points-target]
  (let [{:keys [base type]} (mundane/new)]
    [base (add-enchants base type points-target)]))

(defn >>add []
  (let [{:keys [base type]} (mundane/>>base)]
    (when (and base type)
      (->> (find-valid-enchants-memo base type)
           (r/sample)
           (u/fill-randoms)
           (u/prep-map)))))

(defn >>add-totalling []
  (let [points (some-> (p/>>input "Desired points total:") (parse-long))
        {:keys [base type]} (when points (mundane/>>base))]
    [base
     (add-enchants base type points)]))
