(ns campaign3.curios
  (:refer-clojure :exclude [use])
  (:require (campaign3
              [db :as db]
              [enchants :as e]
              [mundanes :as mundanes]
              [prompting :as p]
              [util :as u])
            [randy.core :as r]))

(def ^:private curios (db/load-all :curios))

(defn- ->inversed [curio]
  (-> curio
      (assoc :multiplier 0)
      (update :effect #(str "Inversed " %))))

(defn new-curio []
  (let [curio (r/sample curios)
        inversed? (u/occurred? 1/3)]
    (-> curio
        (cond-> inversed? ->inversed)
        (dissoc :multiplier))))

(defn use-curios []
  (u/when-let* [{:keys [base type]} (mundanes/choose-base)
                curios-used (let [curios-by-name (u/assoc-by :effect (into curios (map ->inversed) curios))]
                              (some->> (p/>>input "Curios used (maximum 4):"
                                                  (keys curios-by-name)
                                                  :completer :comma-separated)
                                       not-empty
                                       (map curios-by-name)))]
    (let [weightings (reduce
                       (fn [acc {:keys [multiplier effect]}]
                         (if (zero? multiplier)
                           (assoc acc effect 0)
                           (if-let [existing-multiplier (get acc effect)]
                             (assoc acc effect (* 2 existing-multiplier))
                             (assoc acc effect multiplier))))
                       {}
                       curios-used)
          enchants-fn (->> (e/valid-enchants base type)
                           (map (fn [{:keys [tags weighting] :as e}]
                                  (let [new-weighting (transduce
                                                        (comp (map weightings)
                                                              (filter some?)
                                                              (map (fn [multiplier]
                                                                     (* multiplier weighting))))
                                                        (fn
                                                          ([x] x)
                                                          ([x y] (if (and x
                                                                          (or (zero? x) (zero? y)))
                                                                   0
                                                                   (+ (or x 0) y))))
                                                        nil
                                                        tags)]
                                    (assoc e :weighting (or new-weighting weighting)))))
                           u/weighted-sampler)]
      (e/add-enchants-totalling (* 10 (count curios-used))
                                enchants-fn))))
