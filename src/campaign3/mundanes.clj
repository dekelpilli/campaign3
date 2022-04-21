(ns campaign3.mundanes
  (:require [campaign3
             [db :as db]
             [util :as u]
             [prompting :as p]]
            [randy.core :as r]))

(def weapons (db/load-all :weapons))
(def armours (db/load-all :armours))
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
             (u/assoc-by :name (base-types type))
             :sorted? true)))

(defn new []
  (let [type (new-base-type)
        base (case type
               "weapon" (r/sample weapons)
               "armour" (->> (new-armour-slot)
                             (get armours-by-slot)
                             (r/sample)))]
    {:base base
     :type type}))
