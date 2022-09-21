(ns campaign3.helmets
  (:require (campaign3
              [db :as db]
              [prompting :as p]
              [util :as u])
            [randy.core :as r]))

(def ^:private mod-mending-result (r/alias-method-sampler {:upgrade 3 :remove 3 :nothing 4}))

(def character-enchants (as-> (db/load-all :character-enchants) $
                              (group-by :character $)
                              (update-vals $ #(mapv (fn [e] (-> e
                                                                (dissoc :character)
                                                                (update :tags set)))
                                                    %))))

(defn new-helmet []
  (when-let [enchants (p/>>item "Character name:" character-enchants)]
    (loop [total 0
           chosen []
           [{:keys [points] :as enchant} & enchants] (r/shuffle enchants)]
      (let [total (+ points total)]
        (if (>= total 20)
          (conj chosen enchant)
          (recur total (conj chosen enchant) enchants))))))

(defn- select-enchants [character-enchants]
  (not-empty (p/>>distinct-items "Present enchants:" character-enchants)))

(defn- get-present-enchants []
  (some-> (p/>>item "Character name:" character-enchants) select-enchants))

(defn- enchant-levels [enchants]
  (->> enchants
       (map (fn [{:keys [upgradeable effect] :as enchant}]
              (assoc enchant
                :level (if upgradeable
                         (parse-long (p/>>input (str "What is the level of '" effect "'")))
                         1))))
       not-empty))

(defn- get-present-enchants-levels []
  (some-> (get-present-enchants) enchant-levels))

(defn- sum-enchant-points [total {:keys [level points]}]
  (+ total (* level points)))

(defn- fractured-chance [points-total]
  (->> (- 120 points-total)
       (min 100)
       (max 25)
       (- 100)))

(defn- upgrade-helm-mod [present-enchants]
  (let [{:keys [points] :as upgraded-enchant} (r/sample present-enchants)
        points-total (reduce sum-enchant-points 10 present-enchants)
        fractured? (and
                     (<= points 10)
                     (fractured-chance points-total))]
    {:enchant         upgraded-enchant
     :fracture-chance fractured?}))

(defn- add-helm-mod [present-enchants not-present-enchants]
  (let [{:keys [points] :as added-enchant} (r/sample not-present-enchants)
        points-total (reduce sum-enchant-points points present-enchants)]
    {:enchant         added-enchant
     :fracture-chance (fractured-chance points-total)}))

(defn apply-personality []
  (u/when-let* [character-enchants (p/>>item "Character name:" character-enchants)
                present-enchants (some-> (select-enchants character-enchants) enchant-levels)]
    (let [upgradeable-enchants (filterv :upgradeable present-enchants)
          has-upgrades? (seq upgradeable-enchants)
          present-enchant-effects (into #{} (map :effect) present-enchants)
          remaining-mods (filterv (comp not present-enchant-effects :effect) character-enchants)
          has-available-mods? (seq remaining-mods)
          action (r/sample (cond-> []
                                   has-available-mods? (conj :add)
                                   has-upgrades? (conj :upgrade)))
          result (case action
                   :upgrade (upgrade-helm-mod upgradeable-enchants)
                   :add (add-helm-mod present-enchants remaining-mods))]
      (assoc result :action action))))

(defn finish-helmet-progress-upgrade []
  (u/when-let* [present-enchants (get-present-enchants-levels)
                enchant-levels (enchant-levels present-enchants)]
    {:fracture-chance (-> (reduce sum-enchant-points 0 enchant-levels)
                          fractured-chance)}))

(defn mend-helmet []
  (when-let [present-enchants (get-present-enchants-levels)]
    {:enchants (keep (fn [enchant]
                       (case (mod-mending-result)
                         :upgrade (update enchant :level inc)
                         :nothing enchant
                         :remove nil)) present-enchants)}))
