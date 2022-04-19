(ns campaign3.crafting
  (:require
    [campaign3
     [util :as u]
     [enchants :as enchant]
     [db :as db]
     [amounts :as amounts]
     [prompting :as p]
     [mundanes :as mundane]]
    [randy.core :as r]
    [randy.rng :as rng]))

(def crafting-items (->> (db/execute! {:select [:*] :from [:crafting-items]})
                         (map #(update % :amount amounts/amount->fn "1d3"))))

(def crafting-actions
  {:chaos       (fn []
                  (when-let [{:keys [base type]} (mundane/>>base)]
                    {:base     base
                     :enchants (enchant/add-enchants base type (rng/next-int r/default-rng
                                                                             1 51))}))
   :destruction (fn []
                  (let [num-mods (some-> (p/>>input "How many mods on the item?")
                                         (parse-long))]
                    (when (and num-mods (pos? num-mods))
                      (format "Remove mod number %s"
                              (rng/next-int r/default-rng (inc num-mods))))))
   :creation    (fn []
                  (when-let [{:keys [base type]} (mundane/>>base)]
                    {:base     base
                     :enchants (enchant/add-enchants
                                 base type
                                 (max 5
                                      (+ (rng/next-int r/default-rng 21)
                                         (rng/next-int r/default-rng 21))))}))
   :exalted     (fn []
                  (when-let [{:keys [base type]} (mundane/>>base)]
                    (-> (enchant/find-valid-enchants-memo base type)
                        (r/sample)
                        (u/fill-randoms))))})

(defn new []
  (u/get-rand-amount crafting-items))

(defn >>use []
  (when-let [choice (p/>>item "Crafting item:" crafting-actions)]
    (choice)))
