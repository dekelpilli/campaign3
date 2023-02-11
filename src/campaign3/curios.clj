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

(defn- prep-curio [{:keys [effect] :as curio} inversed?]
  (if inversed?
    (assoc curio :multiplier 0 :name (str "Inversed " effect))
    (assoc curio :name effect)))

(defn new-curio []
  (-> (r/sample curios)
      (prep-curio (u/occurred? 1/3))
      (select-keys [:name])))

(defn use-curios []
  (u/when-let* [{:keys [base type]} (mundanes/choose-base)
                curios-used (let [curios-by-name (as-> (map #(prep-curio % false) curios) $
                                                       (into $ (map #(prep-curio % true)) curios)
                                                       (u/assoc-by :name $))]
                              (some-> (p/>>input "Curios used (maximum 4):"
                                                 curios-by-name
                                                 :completer :comma-separated)
                                      not-empty))]
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
