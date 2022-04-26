(ns campaign3.core
  (:gen-class)
  (:require [campaign3
             [enchants :as e]
             [relics :as relics]
             [amulets :as amulets]
             [mundanes :as mundanes]
             [crafting :as crafting]
             [consumables :as consumables]
             [paths :as paths]
             [curios :as curios]
             [rings :as rings]
             [uniques :as uniques]]
            [randy.core :as r]
            [randy.rng :as rng]))

(def loot-actions
  {1  {:name   "1-10 gold"
       :action #(str (rng/next-int r/default-rng 1 11) " gold")}
   3  {:name   "Mundane item"
       :action (comp :base mundanes/new)}
   4  {:name   "Consumable"
       :action consumables/new}
   5  {:name   "Unique"
       :action uniques/new}
   6  {:name   "Low value enchanted item (10 points)"
       :action #(e/random-enchanted 10)}
   7  {:name   "100-150 gold"
       :action #(str (rng/next-int r/default-rng 100 151) " gold")}
   8  {:name   "Non-synergy ring"
       :action rings/new-non-synergy}
   9  {:name   "Synergy ring"
       :action rings/new-synergy}
   10 {:name   "Enchanted item (20 points)"
       :action #(e/random-enchanted 20)}
   11 {:name   "High value enchanted item (30 points)"
       :action #(e/random-enchanted 30)}
   12 {:name   "Curios"
       :action #(cons
                  (mundanes/new)
                  (repeatedly 3 curios/new))}
   13 {:name   "Crafting item"
       :action crafting/new}
   14 {:name   "Amulet"
       :action amulets/new}
   15 {:name   "Prayer stone"
       :action paths/new-divine-dust}
   16 {:name   "New relic"
       :action relics/&new!}})

(defn loot [n]
  (when-let [{:keys [action]} (get loot-actions n)]
    (action)))

(defn loots [& ns]
  (map loot ns))

(defn start []
  #_(let [loot-action-names (->> loot-actions
                                 (map (fn [[k {:keys [name]}]] [k name]))
                                 (into (sorted-map)))]
      (u/display-pairs loot-action-names {:sort? true})
      (loop [action (atom nil)]
        (try
          (let [input (read-line)
                num-input (u/->num input)
                pos-num? (and num-input (pos? num-input))
                dice-input (when-not pos-num? (dice/parse input))
                _ (reset! action (or (:action (loot-actions num-input))
                                     (when dice-input #(dice/roll dice-input))
                                     (when pos-num? (constantly loot-action-names))))
                result (when @action (@action))]
            (cond
              (string? result) (println result)
              (map? result) (u/display-pairs result {:sort? (sorted? result)})
              (seqable? result) (run! u/display-multi-value result)
              :else (when result (u/display-multi-value result))))
          (catch Exception e
            (log/errorf e "Unexpected error")))
        (when @action
          (recur action)))))

(defn -main [& _]
  (println "starting")
  (start))
