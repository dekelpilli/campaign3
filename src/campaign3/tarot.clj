(ns campaign3.tarot
  (:require [campaign3.db :as db]
            [campaign3.util :as u]
            [campaign3.randoms :as randoms]))

(def sampler (->> (db/load-all :tarot-cards)
                  (mapv #(update % :random (fn [random] (when random (randoms/random->fn random)))))
                  u/weighted-sampler))

(defn new []
  (let [{:keys [random] :as card} (sampler)
        val (when random (random))]
    (cond-> (dissoc card :random)
            val (-> (update :name #(format % val))
                    (update :effect #(format % val))))))
