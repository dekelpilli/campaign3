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

(defn- base-match? [actual req]
  (when req
    (= req actual)))

(defn- range-match? [actual req]
  (when (some? req)
    (= req (some? actual))))

(defn prohibits? [{:keys [range] :as base}
                  given-base-type
                  {:keys [base-type ranged?] :as prohibits}]
  (or (false? (base-match? given-base-type base-type))
      (false? (range-match? range ranged?))
      ;TODO :disadvantaged-stealth matcher
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
  (and (not (false? (base-match? given-base-type base-type)))
       (not (false? (range-match? range ranged?)))
       ;TODO :disadvantaged-stealth matcher
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

(defn find-valid-enchants [base base-type] ;TODO memoize, update to new enchant validation syntax (:requires/:prohibits)
  (filter #(compatible? base base-type %) enchants))

(defn add-enchants [base type points-target]
  (let [valid-enchants (->> (find-valid-enchants base type)
                            (shuffle))
        sum (atom 0) ;TODO refactor to not use atom, refactor to allow weighted enchants
        enchants (->> valid-enchants
                      (filter #(and (< @sum points-target)
                                    (swap! sum (partial + (:points %)))))
                      (map (comp u/prep-map u/fill-randoms)))]
    [base enchants]))

(defn random-enchanted [points-target]
  (let [{:keys [base type]} (mundane/new)]
    (add-enchants base type points-target)))

(defn >>add []
  (let [{:keys [base type]} (mundane/>>base)]
    (when (and base type)
      (->> (find-valid-enchants base type)
           (r/sample)))))

(defn add-totalling [^long points]
  (when (pos-int? points)
    (when-let [{:keys [base type]} (mundane/>>base)]
      (-> (add-enchants base type points)
          (second)))))

(defn >>add-totalling []
  (or (some-> (p/>>input "Desired points total:") (parse-long) (add-totalling))
      []))
