(ns campaign3.crafting
  (:require
    [campaign3
     [util :as util]
     [enchants :as enchant]
     [db :as db]
     [amounts :as amounts]
     [prompting :as p]
     [mundanes :as mundane]]))

(def crafting-items (->> (db/execute! {:select [:*] :from [:crafting-items]})
                         (map #(update % :amount amounts/amount->fn "1d3"))))

(def crafting-actions
  {:chaos       (fn []
                  (let [{:keys [base type]} (mundane/>>base)]
                    {:base     base
                     :enchants (enchant/add-enchants base type
                                                     (- (rand-int 51)))}))
   :destruction (fn []
                  (let [num-mods (some-> (p/>>input "How many mods on the item?")
                                         (parse-long))]
                    (when (and num-mods (pos? num-mods))
                      (format "Remove mod number %s"
                              (rand-int (inc num-mods))))))
   :creation    (fn []
                  (let [{:keys [base type]} (mundane/>>base)]
                    {:base     base
                     :enchants (enchant/add-enchants base type
                                                     (max 5
                                                          (+ (rand-int 21) (rand-int 21))))}))
   :annexation  (fn []
                  (when-let [{:keys [base type]} (mundane/>>base)]
                    (-> (enchant/find-valid-enchants base type)
                        (util/rand-enabled)
                        (util/fill-randoms))))
   :exalted     (fn []
                  (let [{:keys [base type]} (mundane/>>base)]
                    (->> (enchant/find-valid-enchants base type)
                         (filter #(pos? (:points % enchant/default-points)))
                         (util/rand-enabled))))})

(defn new []
  (util/get-rand-amount crafting-items))

(defn &use []
  (when-let [choice (p/>>item crafting-actions)]
    (choice)))
