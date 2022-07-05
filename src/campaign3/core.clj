(ns campaign3.core
  (:gen-class)
  (:require (campaign3
              [amulets :as amulets]
              [crafting :as crafting]
              [curios :as curios]
              [enchants :as e]
              [helmets :as helmets]
              [mundanes :as mundanes]
              [paths :as paths]
              [relics :as relics]
              [rings :as rings]
              [uniques :as uniques])
            [randy.core :as r]
            [randy.rng :as rng]))

(def loot-actions
  {1  {:name   "20-30 gold"
       :action #(str (rng/next-int r/default-rng 20 31) " gold")}
   2  {:name   "Unique"
       :action uniques/new}
   3  {:name   "Amulet" ;TODO swap unique and amulet?
       :action amulets/new}
   4  {:name   "Non-synergy ring"
       :action rings/new-non-synergy}
   5  {:name   "Synergy ring"
       :action rings/new-synergy}
   6  {:name   "Enchanted item (high value, 30 points)" ;TODO swap enchanted + special base?
       :action #(e/random-enchanted 30)}
   7  {:name   "Special mundane armour"
       :action mundanes/new-special-armour}
   8  {:name   "Helmet"
       :action helmets/new}
   9  {:name   "Curios"
       :action #(cons
                  (mundanes/new)
                  (repeatedly 4 curios/new))}
   10 {:name   "Crafting item"
       :action crafting/new}
   11 {:name   "Prayer stone" ;TODO reduce to 8 per path
       :action paths/new-divine-dust}
   12 {:name   "New relic"
       :action relics/&new!}})

(defn loot [n]
  (when-let [{:keys [action]} (get loot-actions n)]
    (action)))

(defn loots [& ns]
  (map loot ns))
