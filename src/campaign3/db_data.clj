(ns campaign3.db-data
  (:require [clojure.java.io :as io]
            [campaign3.db :as db]
            [campaign3.util :as u]
            [clojure.set :as set])
  (:import (java.io PushbackReader)))

(defn- load-data [type]
  (with-open [r (PushbackReader. (io/reader (str "db/initial-data/" type ".edn")))]
    (binding [*read-eval* false]
      (read r))))

(defn create-armours! []
  (db/execute! {:create-table :armours
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:ac :integer [:not nil]]
                               [:type :text [:not nil]]
                               [:slot :text [:not nil]]
                               [:disadvantaged-stealth :boolean [:not nil]]]}))


(defn insert-armours! []
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

(defn insert-data! []
  (db/in-transaction
    (insert-armours!)
    (insert-weapons!)
    (insert-uniques!)))
