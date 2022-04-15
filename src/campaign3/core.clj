(ns campaign3.core
  (:gen-class)
  (:require [campaign3
             [util :as u]
             [prompting :as p]
             [relics :as relics]
             [mundanes :as mundane]
             [enchants :as e]
             [crafting :as crafting]
             [consumables :as consumables]
             [prayers :as prayers]
             [encounters :as encounter]
             [dice :as dice]
             [rings :as rings]
             [uniques :as unique]
             [riddle :as riddle]]
            [clojure.tools.logging :as log]))

(defn _req []
  (require '[campaign3
             [util :as u]
             [prompting :as p]
             [relics :as relics]
             [mundanes :as mundane]
             [enchants :as e]
             [crafting :as crafting]
             [consumables :as consumables]
             [prayers :as prayers]
             [encounters :as encounter]
             [dice :as dice]
             [rings :as rings]
             [uniques :as unique]
             [riddle :as riddle]]))

(def loot-actions
  {-1 {:name "Exit"}
   1  {:name   "1-10 gold"
       :action #(str (inc (rand-int 10)) " gold")}
   2  {:name   "Riddle"
       :action riddle/new!}
   3  {:name   "Mundane item"
       :action (comp :base mundane/new)}
   5  {:name   "Consumable"
       :action consumables/new}
   6  {:name   "Unique"
       :action unique/new}
   7  {:name   "Low value enchanted item (10 points)"
       :action #(e/random-enchanted 10)}
   8  {:name   "100-150 gold"
       :action #(str (+ 100 (rand-int 51)) " gold")}
   9  {:name   "Non-synergy ring"
       :action rings/new-non-synergy}
   10 {:name   "Synergy ring"
       :action rings/new-synergy}
   11 {:name   "Enchanted item (20 points)"
       :action #(e/random-enchanted 20)}
   12 {:name   "High value enchanted item (30 points)"
       :action #(e/random-enchanted 30)}
   13 {:name   "Crafting item"
       :action crafting/new}
   15 {:name   "Prayer stone"
       :action prayers/new-stone}
   16 {:name   "New relic"
       :action relics/&new!}
   18 {:name   "Level a relic"
       :action relics/&level-relic!}
   19 {:name   "Progress a prayer path"
       :action prayers/&progress-path!}
   21 {:name   "Add a modifier to an existing item"
       :action e/&add}
   22 {:name   "Add modifiers to an existing items with the given total"
       :action e/&add-totalling}
   23 {:name   "Perform a ring sacrifice"
       :action rings/&sacrifice}
   24 {:name   "Sell a relic"
       :action relics/>>sell!}
   25 {:name   "Travel"
       :action encounter/&travel}
   26 {:name   "Calculate loot rewards"
       :action encounter/&rewards}
   27 {:name   "Positive encounter"
       :action encounter/new-positive}
   28 {:name   "Generate dungeon template"
       :action encounter/new-dungeon}
   29 {:name   "Use a crafting item"
       :action crafting/&use}})

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
