(ns campaign3.util
  (:require [campaign3.db :as db]
            [randy.core :as r]
            [randy.rng :as rng]))

(def session)

(defn record! [event amount]
  (when (bound? #'session)
    (db/execute! {:insert-into   :analytics
                  :values        [{:type    event
                                   :session session
                                   :amount  amount}]
                  :on-conflict   [:type :session]
                  :do-update-set {:amount [:+ :EXCLUDED.amount amount]}})))

(defn set-session! [n]
  (alter-var-root #'session (constantly n)))

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
      fill-randoms))

(defn assoc-by [f coll]
  (into {} (map (juxt f identity)) coll))
