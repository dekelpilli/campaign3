(ns campaign3.core
  (:gen-class)
  (:require (campaign3
              [amulets :as amulets]
              [crafting :as crafting]
              [curios :as curios]
              [enchants :as e]
              [helmets :as helmets]
              [mundanes :as mundanes]
              [relics :as relics]
              [rings :as rings]
              [tarot :as tarot]
              [uniques :as uniques]
              [util :as u])
            [randy.core :as r]
            [randy.rng :as rng]
            [puget.printer :as puget]))

(def loot-actions
  {1  {:name   "20-30 gold"
       :action (fn gold-loot [] (str (rng/next-int r/default-rng 20 31) " gold"))}
   2  {:name   "Unique"
       :action uniques/new}
   3  {:name   "Amulet"
       :action amulets/new}
   4  {:name   "Rings"
       :action #(rings/new-rings 2)}
   5  {:name   "Enchanted item"
       :action (fn enchanted-loot [] (e/random-enchanted 30))}
   6  {:name   "Curios"
       :action (fn curios-loot [] (cons (mundanes/new) (repeatedly 4 curios/new)))}
   7  {:name   "Special mundane armour"
       :action mundanes/new-special-armour}
   8  {:name   "Crafting item"
       :action crafting/new}
   9  {:name   "Helmet"
       :action helmets/new}
   10 {:name   "Tarot card"
       :action (fn tarot-loot []
                 (dotimes [_ 3] (when-let [card (tarot/lookup)]
                                  (puget/cprint card))))}
   11 {:name   "New relic"
       :action relics/new!}
   12 {:name   "Divine dust"
       :action (constantly "Divine Dust")}})

(defn loot* [n]
  (when-let [{:keys [action]} (get loot-actions n)]
    (action)))

(defn loot [n]
  (u/record! (str "loot:" n) 1)
  (loot* n))

(defn loots* [ns]
  (mapv (fn collect-loot [n]
          (let [{:keys [name action]} (get loot-actions n)]
            {:name   (str name " (" n ")")
             :result (when action (action))}))
        ns))

(defn loots [& ns]
  (doseq [[n amount] (frequencies ns)]
    (u/record! (str "loot:" n) amount))
  (loots* ns))
