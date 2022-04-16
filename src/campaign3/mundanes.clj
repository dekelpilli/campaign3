(ns campaign3.mundanes
  (:require [campaign3
             [db :as db]
             [util :as util]]
            [randy.core :as r]
            [campaign3.prompting :as p]
            [campaign3.util :as u]))

(def weapons (db/execute! {:select [:*] :from [:weapons]}))
(def armours (db/execute! {:select [:*] :from [:armours]}))

(def base-types {"weapon" weapons "armour" armours})

(defn >>base
  ([]
   (let [choice (p/>>item "Base category:" base-types)]
     (when choice
       {:base (>>base choice)
        :type choice})))
  ([type]
   (-> (u/assoc-by :name (base-types type))
       (p/>>item "Base type:"))))

(defn new []
  (let [type (if (util/occurred? 2/3) "armour" "weapon")
        mundanes (get base-types type)
        base (case type
               "weapon" (util/rand-enabled mundanes)
               "armour" (let [slot (r/weighted-sample {"body"   3
                                                       "helmet" 3
                                                       "gloves" 3
                                                       "boots"  3
                                                       "shield" 1})]
                          (->> mundanes
                               (filter #(= slot (:slot %)))
                               (r/sample))))]
    {:base base
     :type type}))
