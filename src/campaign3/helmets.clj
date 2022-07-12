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

(defn- select-enchants [character-enchants]
  (not-empty (p/>>distinct-items "Present enchants:" character-enchants)))

(defn- get-present-enchants []
  (some-> (p/>>item "Character name:" character-enchants) (select-enchants)))

(defn- enchant-levels [enchants]
  (->> enchants
       (map (fn [{:keys [upgradeable effect] :as enchant}]
              (assoc enchant
                :level (if upgradeable
                         (parse-long (p/>>input (str "What is the level of '" effect "'")))
                         1))))
       (not-empty)))

(defn- get-present-enchants-levels []
  (some-> (get-present-enchants) (enchant-levels)))

(defn- sum-enchant-points [total {:keys [level points]}]
  (+ total (* level points)))

(defn- fractured? [points-total] ;TODO more fun to let players roll d100 instead of doing it on the backend? if so, round chance to nearest %
  (u/occurred? (- 1 (/ 30 points-total))))

(defn- upgrade-helm-mod [present-enchants]
  (let [{:keys [points] :as upgraded-enchant} (r/sample present-enchants)
        points-total (reduce sum-enchant-points 10 present-enchants)
        fractured? (and
                     (<= points 10)
                     (fractured? points-total))]
    {:enchant    upgraded-enchant
     :fractured? fractured?}))

(defn- add-helm-mod [present-enchants not-present-enchants]
  (let [{:keys [points] :as added-enchant} (r/sample not-present-enchants)
        points-total (reduce sum-enchant-points points present-enchants)]
    {:enchant    added-enchant
     :fractured? (fractured? points-total)}))

(defn apply-personality []
  (when-let [character-enchants (p/>>item "Character name:" character-enchants)]
    (when-let [present-enchants (some-> (select-enchants character-enchants) (enchant-levels))]
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
        (assoc result :action action)))))

(defn finish-progress-upgrade []
  (when-let [present-enchants (get-present-enchants-levels)]
    (when-let [enchant-levels (enchant-levels present-enchants)]
      (let [points-total (reduce sum-enchant-points 0 enchant-levels)]
        {:fractured? (fractured? points-total)}))))

(defn mend []
  (when-let [present-enchants (get-present-enchants-levels)]
    {:enchants (keep (fn [enchant]
                       (case (mod-mending-result)
                         :upgrade (update enchant :level inc)
                         :nothing enchant
                         :remove nil)) present-enchants)}))
