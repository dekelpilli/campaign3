(ns campaign3.helmets
  (:require (campaign3
              [db :as db]
              [enchants :as e]
              [prompting :as p]
              [util :as u])
            [randy.core :as r]))

(def ^:private mod-mending-result (r/alias-method-sampler {:upgrade 3 :remove 3 :nothing 4}))

(def character-enchants (->> (db/load-all :character-enchants)
                             (group-by :character)))

(defn new []
  (when-let [enchants (p/>>item "Character name:" character-enchants)]
    (e/add-enchants 20 #(r/sample enchants))))

(defn- get-present-enchants []
  (when-let [available-enchants (p/>>item "Character name:" character-enchants)]
    (seq (p/>>distinct-items "Present enchants:" available-enchants))))

(defn- get-present-enchants-levels []
  (when-let [present-enchants (get-present-enchants)]
    (seq (map #(assoc % :level
                        (parse-long (p/>>input (str "What is the level of '" (:effect %) "'"))))
              present-enchants))))

(defn- sum-enchant-points [total {:keys [level points]}]
  (+ total (* level points)))

(defn- fractured? [points-total]
  (u/occurred? (- 1 (/ 30 points-total))))

(defn upgrade [] ;TODO add option of adding new random mod...
  (when-let [present-enchants (get-present-enchants-levels)]
    (let [{:keys [points] :as upgraded-enchant} (r/sample present-enchants)
          points-total (reduce sum-enchant-points 10 present-enchants)
          fractured? (and
                       (<= points 10)
                       (fractured? points-total))]
      {:upgraded   upgraded-enchant
       :fractured? fractured?})))

(defn finish-progress-upgrade []
  (when-let [present-enchants (get-present-enchants-levels)]
    (when-let [enchant-levels (map (fn [{:keys [upgradeable effect] :as enchant}]
                                     (assoc enchant
                                       :level (if upgradeable
                                                (parse-long (p/>>input (str "What is the level of '" effect "'")))
                                                1)))
                                   present-enchants)]
      (let [points-total (reduce sum-enchant-points 0 enchant-levels)]
        {:fractured? (fractured? points-total)}))))

(defn mend []
  (when-let [present-enchants (get-present-enchants-levels)]
    {:enchants (keep (fn [enchant]
                       (case (mod-mending-result)
                         :upgrade (update enchant :level inc)
                         :nothing enchant
                         :remove nil)) present-enchants)}))
