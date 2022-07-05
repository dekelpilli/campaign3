(ns campaign3.mundanes
  (:require (campaign3
              [db :as db]
              [prompting :as p]
              [util :as u])
            [randy.core :as r]))

(def weapons (db/load-all :weapons))
(def armours (db/load-all :armours))
(def special-armour-by-slot (->> (db/load-all :special-armours)
                                 (group-by :slot)))
(def armours-by-slot (group-by :slot armours))

(def base-types {"weapon" weapons "armour" armours})

(def new-base-type (r/alias-method-sampler {"armour" 2
                                            "weapon" 1}))
(def new-armour-slot (r/alias-method-sampler {"body"   3
                                              "helmet" 3
                                              "gloves" 3
                                              "boots"  3
                                              "shield" 1}))

(defn >>base
  ([]
   (let [choice (p/>>item "Base category:" (keys base-types))]
     (when choice
       {:base (>>base choice)
        :type choice})))
  ([type]
   (p/>>item "Base type:"
             (u/assoc-by :name (base-types type)))))

(defn new []
  (let [type (new-base-type)
        base (case type
               "weapon" (r/sample weapons)
               "armour" (->> (new-armour-slot)
                             (get armours-by-slot)
                             (r/sample)))]
    {:base base
     :type type}))

(defn new-special-armour []
  (->> (r/sample ["body" "boots" "gloves"])
       (special-armour-by-slot)
       (r/sample)))

(defn new-special-of-slot []
  (when-let [slot (p/>>item ["body" "boots" "gloves"])]
    (->> slot
         (special-armour-by-slot)
         (r/sample))))
