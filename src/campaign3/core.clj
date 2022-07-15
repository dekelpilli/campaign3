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
              [uniques :as uniques]
              [util :as u])
            [randy.core :as r]
            [randy.rng :as rng]))

(def loot-actions
  {1  {:name   "20-30 gold"
       :action (fn gold-loot [] (str (rng/next-int r/default-rng 20 31) " gold"))}
   2  {:name   "Unique"
       :action uniques/new}
   3  {:name   "Amulet"
       :action amulets/new}
   4  {:name   "Non-synergy ring"
       :action rings/new-non-synergy} ;TODO reconsider having two loot rolls for rings
   5  {:name   "Synergy ring"
       :action rings/new-synergy}
   6  {:name   "Enchanted item"
       :action (fn enchanted-loot [] (e/random-enchanted 30))}
   7  {:name   "Special mundane armour"
       :action mundanes/new-special-armour}
   8  {:name   "Curios"
       :action (fn curios-loot [] (cons (mundanes/new) (repeatedly 4 curios/new)))}
   9  {:name   "Helmet"
       :action helmets/new}
   10 {:name   "Crafting item"
       :action crafting/new}
   11 {:name   "New relic"
       :action relics/new!}
   12 {:name   "Prayer stone" ;TODO reduce to 5 per path
       :action paths/new-divine-dust}})

(defn loot [n]
  (u/record! (str "loot:" n) 1)
  (when-let [{:keys [action]} (get loot-actions n)]
    (action)))

(defn loots [& ns]
  (doseq [[n amount] (frequencies ns)]
    (u/record! (str "loot:" n) amount))
  (mapv (fn collect-loot [n]
          (let [{:keys [name action]} (get loot-actions n)]
            {:name   (str name " (" n ")")
             :result (when action (action))}))
        ns))
