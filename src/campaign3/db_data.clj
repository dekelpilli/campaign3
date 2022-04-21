(ns campaign3.db-data
  (:require [clojure.java.io :as io]
            [campaign3.db :as db]
            [campaign3.util :as u]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import (java.io PushbackReader)))

(defn- load-data [type]
  (with-open [r (PushbackReader. (io/reader (str "db/initial-data/" type ".edn")))]
    (binding [*read-eval* false]
      (filter #(:enabled? % true) (read r)))))

(defn- drop! [table]
  (db/execute! {:drop-table [:if-exists table]}))

(defn create-armours! []
  (db/execute! {:create-table :armours
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:ac :integer [:not nil]]
                               [:type :text [:not nil]]
                               [:slot :text [:not nil]]
                               [:disadvantaged-stealth :boolean [:not nil]]]}))


(defn insert-armours! []
  (drop! :armours)
  (create-armours!)
  (let [armours (load-data "armour")]
    (db/execute! {:insert-into [:armours]
                  :values
                  (map (fn [armour]
                         (set/rename-keys armour {:disadvantaged-stealth? :disadvantaged-stealth}))
                       armours)})))

(defn create-weapons! []
  (db/execute! {:create-table :weapons
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:type :text [:not nil]]
                               [:category :text [:not nil]]
                               [:damage :text [:not nil]]
                               [:proficiency :text [:not nil]]
                               [:range :text]
                               [:damage-types :jsonb [:not nil]]
                               [:traits :jsonb [:not nil]]]}))


(defn insert-weapons! []
  (drop! :weapons)
  (create-weapons!)
  (let [weapons (load-data "weapon")]
    (db/execute! {:insert-into [:weapons]
                  :values
                  (map (fn [weapon]
                         (-> weapon
                             (update :damage-types u/jsonb-lift)
                             (update :traits u/jsonb-lift)))
                       weapons)})))


(defn create-uniques! []
  (db/execute! {:create-table :uniques
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:base :text [:not nil]]
                               [:effects :jsonb [:not nil]]
                               [:extras :jsonb]]}))

(defn insert-uniques! []
  (drop! :uniques)
  (create-uniques!)
  (let [uniques (load-data "unique")]
    (db/execute! {:insert-into [:uniques]
                  :values
                  (map (fn [unique]
                         (let [extras (not-empty (dissoc unique :name :base :effects))]
                           (-> unique
                               (update :effects u/jsonb-lift)
                               (select-keys [:name :base :effects])
                               (assoc :extras [:lift extras]))))
                       uniques)})))

(defn create-crafting-items! []
  (db/execute! {:create-table :crafting-items
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:effect :text [:not nil]]
                               [:amount :jsonb]]}))

(defn insert-crafting-items! []
  (drop! :crafting-items)
  (create-crafting-items!)
  (let [crafting-items (load-data "crafting-item")]
    (db/execute! {:insert-into [:crafting-items]
                  :values
                  (->> crafting-items
                       (filter #(:enabled? % true))
                       (map (fn [crafting-item]
                              (-> crafting-item
                                  (dissoc :enabled?)
                                  (update :amount u/jsonb-lift)))))})))

(defn create-consumables! []
  (db/execute! {:create-table :consumables
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:effect :text [:not nil]]
                               [:amount :jsonb]
                               [:randoms :jsonb]]}))

(defn insert-consumables! []
  (drop! :consumables)
  (create-consumables!)
  (let [consumables (load-data "consumable")]
    (db/execute! {:insert-into [:consumables]
                  :values
                  (map (fn [consumable]
                         (-> consumable
                             (update :amount u/jsonb-lift)
                             (update :randoms u/jsonb-lift)))
                       consumables)})))

(defn create-positive-encounters! []
  (db/execute! {:create-table :positive-encounters
                :with-columns [[:rules :text [:primary-key] [:not nil]]
                               [:cost :text [:not nil]]
                               [:participants :text [:not nil]]]}))

(defn insert-positive-encounters! []
  (drop! :positive-encounters)
  (create-positive-encounters!)
  (db/execute! {:insert-into [:positive-encounters]
                :values      (load-data "positive-encounter")}))

(defn create-enchants! []
  (db/execute! {:create-table :enchants
                :with-columns [[:effect :text [:not nil]]
                               [:points :integer [:not nil]]
                               [:upgrade-points :integer [:not nil]]
                               [:upgradeable :boolean [:not nil]]
                               [:requires :jsonb]
                               [:prohibits :jsonb]
                               [:randoms :jsonb]
                               [:tags :jsonb] ;TODO weighted enchants
                               [[:primary-key :effect :points :upgrade-points]]]}))

(defn insert-enchants! []
  (drop! :enchants)
  (create-enchants!)
  (db/execute! {:insert-into [:enchants]
                :values
                (->> (load-data "enchant")
                     (map (fn [{:keys [points upgrade-points upgradeable?]
                                :or   {points 10 upgradeable? true}
                                :as   enchant}]
                            (-> enchant
                                (update :requires u/jsonb-lift)
                                (update :randoms u/jsonb-lift)
                                (update :prohibits u/jsonb-lift)
                                (update :tags (comp u/jsonb-lift vec))
                                (dissoc :upgradeable?)
                                (assoc :points points
                                       :upgradeable upgradeable?
                                       :upgrade-points (or upgrade-points points))))))}))

(defn create-rings! []
  (db/execute! {:create-table :rings
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:effect :text [:not nil]]
                               [:points :integer [:not nil]]
                               [:randoms :jsonb]
                               [:synergy :boolean [:not nil]]]}))

(defn insert-rings! []
  (drop! :rings)
  (create-rings!)
  (db/execute! {:insert-into [:rings]
                :values
                (->> (load-data "ring")
                     (map (fn [{:keys [name] :as ring}]
                            (-> ring
                                (update :randoms u/jsonb-lift)
                                (assoc :synergy (str/starts-with? name "The"))))))}))

(defn create-curios! []
  (db/execute! {:create-table :curios
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:tag :text [:not nil]]
                               [:multiplier :integer [:not nil]]]}))

(defn insert-curios! []
  (drop! :curios)
  (create-curios!)
  (db/execute! {:insert-into [:curios]
                :values (load-data "curio")}))

(defn insert-data! []
  (db/in-transaction
    (insert-armours!)
    (insert-weapons!)
    (insert-uniques!)
    (insert-crafting-items!)
    (insert-consumables!)
    (insert-positive-encounters!)
    (insert-enchants!)
    (insert-rings!)
    (insert-curios!)))

(defn backup-data! []
  ;TODO write any mutable data to db/current-state to keep log of changes by session
  )
