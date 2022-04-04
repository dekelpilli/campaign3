(ns campaign3.core
  (:gen-class)
  (:require [campaign3
             [util :as util]
             [relic :as relic :refer [&level-relic!]]
             [mundane :as mundane]
             [enchant :as enchant :refer [add-totalling]]
             [crafting :as crafting]
             [consumable :as consumable]
             [prayer :as prayer :refer [&progress-path!]]
             [monster :as monster]
             [miscreation :as miscreation]
             [encounter :as encounter :refer [rewards]]
             [dice :as dice :refer [roll]]
             [ring :as ring :refer [&sacrifice]]
             [unique :as unique]
             [riddle :as riddle]
             [state :as state :refer [reload!]]]
            [clojure.tools.logging :as log]))

(def loot-actions
  {-1 {:name "Exit"}
   1  {:name   "1-10 gold"
       :action #(str (inc (rand-int 10)) " gold")}
   2  {:name   "Riddle"
       :action riddle/new!}
   3  {:name   "Mundane item"
       :action #(:base (mundane/new))}
   4  {:name   "Miscreation"
       :action miscreation/new}
   5  {:name   "Consumable"
       :action consumable/new}
   6  {:name   "Unique"
       :action unique/new}
   7  {:name   "Low value enchanted item (10 points)"
       :action #(enchant/random-enchanted 10)}
   8  {:name   "100-150 gold"
       :action #(str (+ 100 (rand-int 51)) " gold")}
   9  {:name   "Non-synergy ring"
       :action ring/new-non-synergy}
   10 {:name   "Synergy ring"
       :action ring/new-synergy}
   11 {:name   "Enchanted item (20 points)"
       :action #(enchant/random-enchanted 20)}
   12 {:name   "High value enchanted item (30 points)"
       :action #(enchant/random-enchanted 30)}
   13 {:name   "Crafting item"
       :action crafting/new}
   14 {:name   "Amulet"
       :action monster/generate-amulet}
   15 {:name   "Prayer stone"
       :action prayer/new-stone}
   16 {:name   "New relic"
       :action relic/&new!}
   17 {:name   "Reload data from files"
       :action state/reload!}
   18 {:name   "Level a relic"
       :action relic/&level-relic!}
   19 {:name   "Progress a prayer path"
       :action prayer/&progress-path!}
   20 {:name   "Choose monsters from given CRs"
       :action monster/&new}
   21 {:name   "Add a modifier to an existing item"
       :action enchant/&add}
   22 {:name   "Add modifiers to an existing items with the given total"
       :action enchant/&add-totalling}
   23 {:name   "Perform a ring sacrifice"
       :action ring/&sacrifice}
   24 {:name   "Sell a relic"
       :action relic/&sell!}
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
  (let [loot-action-names (->> loot-actions
                               (map (fn [[k {:keys [name]}]] [k name]))
                               (into (sorted-map)))]
    (util/display-pairs loot-action-names {:sort? true})
    (loop [action (atom nil)]
      (try
        (let [input (read-line)
              num-input (util/->num input)
              pos-num? (and num-input (pos? num-input))
              dice-input (when-not pos-num? (dice/parse input))
              _ (reset! action (or (:action (loot-actions num-input))
                                   (when dice-input #(dice/roll dice-input))
                                   (when pos-num? (constantly loot-action-names))))
              result (when @action (@action))]
          (cond
            (string? result) (println result)
            (map? result) (util/display-pairs result {:sort? (sorted? result)})
            (seqable? result) (run! util/display-multi-value result)
            :else (when result (util/display-multi-value result))))
        (catch Exception e
          (log/errorf e "Unexpected error")))
      (when @action
        (recur action)))))

(defn -main [& _]
  (println "starting")
  (start))
