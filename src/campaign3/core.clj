(ns campaign3.core
  (:gen-class)
  (:require (campaign3
              [amulets :as amulets]
              [crafting :as crafting]
              [curios :as curios]
              [db :as db]
              [enchants :as e]
              [helmets :as helmets]
              [mundanes :as mundanes]
              [paths :as paths]
              [relics :as relics]
              [rings :as rings]
              [uniques :as uniques])
            [randy.core :as r]
            [randy.rng :as rng]))

(def session)

(def loot-actions
  {1  {:name   "20-30 gold"
       :action (fn gold-loot [] (str (rng/next-int r/default-rng 20 31) " gold"))}
   2  {:name   "Unique"
       :action uniques/new}
   3  {:name   "Amulet"
       :action amulets/new}
   4  {:name   "Non-synergy ring"
       :action rings/new-non-synergy}
   5  {:name   "Synergy ring"
       :action rings/new-synergy}
   6  {:name   "Enchanted item (high value, 30 points)" ;TODO swap enchanted + special base?
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

(defn set-session! [n]
  (alter-var-root #'session (constantly n))
  (db/execute! {:insert-into :loot-analytics
                :values      (map (fn [roll] {:session n :roll roll :amount 0}) (keys loot-actions))}))

(defn loot [n]
  (when (bound? #'session)
    (db/execute! {:update :loot-analytics
                  :set    {:amount [:+ :amount 1]}
                  :where  [:and
                           [:= :session session]
                           [:= :roll n]]}))
  (when-let [{:keys [action]} (get loot-actions n)]
    (action)))

(defn loots [& ns]
  (mapv loot ns))
