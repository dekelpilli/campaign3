(ns campaign3.util
  (:require [campaign3.db :as db]
            [randy.core :as r]
            [randy.rng :as rng]))

(def session)

(defn set-session! [n]
  (alter-var-root #'session (constantly n))
  (db/execute! {:insert-into :analytics
                :values      (map (fn [roll] {:session n :type (str "loot:" roll) :amount 0}) (range 1 13))
                :on-conflict {}
                :do-nothing  true})
  (db/execute! {:insert-into :analytics
                :values      (map (fn [encounter-type] {:session n :type (str "encounter:" encounter-type) :amount 0})
                                  ["random" "positive"])
                :on-conflict {}
                :do-nothing  true}))

(defn jsonb-lift [x]
  (when x [:lift x]))

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
