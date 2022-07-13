(ns campaign3.db-data
  (:require [campaign3.db :as db]
            [campaign3.util :as u]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [jsonista.core :as j]
            [clojure.pprint :as pprint])
  (:import (java.io File PushbackReader)))

(defn- load-data [type]
  (with-open [r (PushbackReader. (io/reader (str "db/initial-data/" type ".edn")))]
    (binding [*read-eval* false]
      (filter #(:enabled? % true) (read r)))))

(defn write-data! [path coll]
  (with-open [writer (io/writer path)]
    (pprint/pprint coll writer)))

(defn find-cr [cr]
  (when-let [cr (edn/read-string (if (map? cr) (:cr cr) cr))]
    (double cr)))

(defn find-type [type]
  (if (map? type) (:type type) type))

(defn prepare-monster [mon]
  (-> mon
      (update :cr find-cr)
      (update :type find-type)
      (select-keys [:name :source :page :type :cr :trait])))

(defn load-json-file [file]
  (-> (slurp file)
      (j/read-value j/keyword-keys-object-mapper)))

(defn load-monsters []
  (let [files (->> "5et/monsters"
                   (File.)
                   (file-seq)
                   (remove #(.isDirectory %)))
        mons (->> files
                  (map load-json-file)
                  (map :monster)
                  (flatten)
                  (sequence
                    (comp (remove #(contains? % :_copy))
                          (map prepare-monster)
                          (filter :cr)
                          (filter (comp seq :trait)))))]
    mons))

(defn drop! [table]
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
                               [:tags :jsonb]
                               [[:primary-key :effect :points :upgrade-points]]]}))

(defn insert-enchants! []
  (drop! :enchants)
  (create-enchants!)
  (db/execute! {:insert-into [:enchants]
                :values
                (->> (load-data "enchant")
                     (map (fn [{:keys [points upgrade-points]
                                :or   {points 10}
                                :as   enchant}]
                            (-> enchant
                                (update :requires u/jsonb-lift)
                                (update :randoms u/jsonb-lift)
                                (update :prohibits u/jsonb-lift)
                                (update :tags (comp u/jsonb-lift vec))
                                (update :upgradeable (complement false?))
                                (assoc :points points
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
                :values      (load-data "curio")}))

(defn create-divinity-paths! []
  (db/execute! {:create-table :divinity-paths
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:levels :jsonb [:not nil]]]}))

(defn insert-divinity-paths! []
  (drop! :divinity-paths)
  (create-divinity-paths!)
  (db/execute! {:insert-into [:divinity-paths]
                :values      (->> (load-data "divinity-path")
                                  (map #(update % :levels u/jsonb-lift)))}))


(defn create-character-enchants! []
  (db/execute! {:create-table :character-enchants
                :with-columns [[:effect :text [:not nil]]
                               [:character :text [:not nil]]
                               [:points :integer [:not nil]]
                               [:upgradeable :boolean [:not nil]]
                               [[:primary-key :effect :character]]]}))

(defn insert-character-enchants! []
  (drop! :character-enchants)
  (create-character-enchants!)
  (db/execute! {:insert-into [:character-enchants]
                :values      (->> (load-data "character-enchant")
                                  (reduce
                                    (fn [acc [character enchants]]
                                      (into acc
                                            (map (fn [enchant]
                                                   (-> enchant
                                                       (assoc :character (name character))
                                                       (update :upgradeable (complement false?)))))
                                            enchants))
                                    []))}))


(defn create-special-armours! []
  (db/execute! {:create-table :special-armours
                :with-columns [[:name :text [:primary-key] [:not nil]]
                               [:effect :text [:not nil]]
                               [:slot :text [:not nil]]]}))

(defn insert-special-armours! []
  (drop! :special-armours)
  (create-special-armours!)
  (db/execute! {:insert-into [:special-armours]
                :values      (load-data "special-armour")}))

(defn create-monsters! []
  (db/execute! {:create-table :monsters
                :with-columns [[:name :text [:not nil]]
                               [:book :text [:not nil]]
                               [:page :integer [:not nil]]
                               [:type :text [:not nil]]
                               [:cr :real [:not nil]]
                               [:traits :jsonb [:not nil]]
                               [[:primary-key :cr :name]]]}))

(defn insert-monsters! []
  (drop! :monsters)
  (create-monsters!)
  (db/execute! {:insert-into [:monsters]
                :values      (->> (load-monsters)
                                  (map (fn [{:keys [trait source] :as monster}]
                                         (-> monster
                                             (dissoc :source :trait)
                                             (assoc :book source
                                                    :traits (u/jsonb-lift trait))))))}))

(defn create-analytics! []
  (db/execute! {:create-table :analytics
                :with-columns [[:type :text [:not nil]]
                               [:session :integer [:not nil]]
                               [:amount :integer [:not nil]]
                               [[:primary-key :type :session]]]}))

(defn insert-data! []
  (db/in-transaction
    (transduce (map (comp :next.jdbc/update-count first)) + 0
               [(insert-armours!)
                (insert-weapons!)
                (insert-uniques!)
                (insert-crafting-items!)
                (insert-positive-encounters!)
                (insert-enchants!)
                (insert-rings!)
                (insert-curios!)
                (insert-divinity-paths!)
                (insert-character-enchants!)
                (insert-special-armours!)])))

(defn backup-table! [table]
  (->> (db/load-all table)
       (write-data! (str "db/current-state/" (name table) ".edn"))))

(defn backup-data! []
  (run! backup-table! [:divinity-progress :relics :loot-analytics]))
