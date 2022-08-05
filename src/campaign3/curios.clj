(ns campaign3.curios
  (:refer-clojure :exclude [use])
  (:require (campaign3
              [db :as db]
              [enchants :as e]
              [mundanes :as mundanes]
              [prompting :as p]
              [util :as u])
            [randy.core :as r]))

(def curios (db/load-all :curios))

(defn- ->inversed [curio]
  (-> curio
      (assoc :multiplier 0)
      (update :name #(str "Inversed " %))))

(defn new []
  (let [curio (r/sample curios)
        inversed? (u/occurred? 1/3)]
    (-> curio
        (cond-> inversed? ->inversed)
        (dissoc :multiplier))))

(defn use []
  (when-let [{:keys [base type]} (mundanes/choose-base)]
    (when-let [curios-used (let [curios-by-name (u/assoc-by :name (into curios (map ->inversed) curios))]
                             (some->> (p/>>input "Curios used (maximum 4):"
                                                 (keys curios-by-name)
                                                 :completer :comma-separated)
                                      not-empty
                                      (map curios-by-name)))]
      (let [weightings (reduce
                         (fn [acc {:keys [multiplier tag]}]
                           (if (zero? multiplier)
                             (assoc acc tag 0)
                             (if-let [existing-multiplier (get acc tag)]
                               (assoc acc tag (* 2 existing-multiplier))
                               (assoc acc tag multiplier))))
                         {}
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
                             u/weighted-sampler)]
        (e/add-enchants (* 10 (count curios-used))
                        enchants-fn)))))
