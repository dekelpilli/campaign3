(ns campaign3.curios
  (:require [randy.core :as r]
            [campaign3
             [db :as db]
             [util :as u]
             [prompting :as p]
             [enchants :as e]
             [mundanes :as mundanes]]
            [clojure.set :as set]))

(def curios (db/load-all :curios))

(defn- ->inversed [curio]
  (-> curio
      (assoc :multiplier 0)
      (update :name #(str "Inversed " %))))

(defn new []
  (let [curio (r/sample curios)
        inversed? (u/occurred? 1/3)]
    (cond-> curio
            inversed? (->inversed))))

(defn >>use []
  (when-let [{:keys [base type]} (mundanes/>>base)]
    (when-let [curios-used (->> (into curios (map ->inversed) curios)
                                (u/assoc-by :name)
                                (p/>>checkbox "Curios used:")
                                (not-empty))]
      (let [weightings (reduce
                         (fn [acc {:keys [multiplier tag]}]
                           (if (zero? multiplier)
                             (assoc acc tag 0)
                             (if-let [existing-multiplier (get acc tag)]
                               (update acc tag (* 2 existing-multiplier))
                               (assoc acc tag multiplier))))
                         curios-used)
            enchants-fn (->> (e/valid-enchants base type)
                             (map (fn [{:keys [tags weighting] :as e}]
                                    (->> (transduce
                                           (comp (map weightings)
                                                 (filter some?))
                                           *
                                           weighting
                                           tags)
                                         (assoc e :weighting))))
                             (e/->valid-enchant-fn))]
        (e/add-enchants (* 10 (count curios-used))
                        enchants-fn)))))
