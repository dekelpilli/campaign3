(ns campaign3.mundanes
  (:require (campaign3
              [db :as db]
              [prompting :as p]
              [randoms :as randoms]
              [util :as u])
            [randy.core :as r]))

(def ^:private weapons (db/load-all :weapons))
(def ^:private armours (db/load-all :armours))
(def ^:private special-armours (->> (db/load-all :special-armours)
                                    (mapv #(update % :randoms randoms/randoms->fn))))
(def ^:private special-armour-by-slot (group-by :slot special-armours))
(def ^:private armours-by-slot (group-by :slot armours))

(def ^:private base-types {"weapon" weapons "armour" armours})

(def ^:private new-base-type (r/alias-method-sampler {"armour" 2
                                                      "weapon" 1}))
(def ^:private new-armour-slot (r/alias-method-sampler {"body"   3
                                                        "gloves" 3
                                                        "boots"  3
                                                        "shield" 1}))

(defn choose-base
  ([]
   (u/when-let* [base-type (p/>>item "Base category:" (keys base-types))
                 base (choose-base base-type)]
     {:base base
      :type base-type}))
  ([type]
   (p/>>item "Base type:"
             (u/assoc-by :name (base-types type)))))

(defn name->base [type name]
  (->> (get base-types type)
       (filter (comp #{name} :name))
       first))

(defn new-mundane []
  (let [type (new-base-type)
        base (case type
               "weapon" (r/sample weapons)
               "armour" (->> (new-armour-slot)
                             (get armours-by-slot)
                             r/sample))]
    {:base base
     :type type}))

(defn new-special-armour []
  (-> (r/sample ["body" "boots" "gloves"]) special-armour-by-slot r/sample u/fill-randoms))

(defn new-special-of-slot []
  (when-let [slot (p/>>item ["body" "boots" "gloves"])]
    (-> slot special-armour-by-slot r/sample u/fill-randoms)))

(defn sample-special-armours []
  (when-let [amount (some-> (p/>>input "How many bases?")
                            parse-long
                            (min (count special-armours)))]
    (->> (r/sample-without-replacement amount special-armours)
         (map u/fill-randoms))))
