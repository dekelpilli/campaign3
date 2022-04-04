(ns campaign3.state
  (:require [clojure.pprint :as pprint]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)))

(def ^:private path "resources/")
(def ^:private relic-key-order {:name       0
                                :type       1
                                :owner      2
                                :base       3
                                :existing   4
                                :available  5
                                :progressed 6
                                :enabled?   7
                                :found?     8
                                :level      9})

(defn load-data [type]
  (with-open [r (PushbackReader. (io/reader (str path type ".edn")))]
    (binding [*read-eval* false]
      (read r))))

(defn write-data! [a type]
  (with-open [writer (io/writer (str path type ".edn"))]
    (pprint/pprint @a writer)))

(def relics (atom nil))
(def prayer-paths (atom nil))
(def prayer-progressions (atom nil))
(def crafting-items (atom nil))
(def weapons (atom nil))
(def armours (atom nil))
(def enchants (atom nil))
(def rings (atom nil))
(def consumables (atom nil))
(def miscreations (atom nil))
(def monsters (atom nil))
(def uniques (atom nil))
(def character-enchants (atom nil))
(def positive-encounters (atom nil))
(def riddles (atom nil))

(defn reload! []
  (println "Loading...")
  (reset! relics (load-data "relic"))
  (reset! prayer-paths (load-data "prayer-path"))
  (reset! prayer-progressions (load-data "prayer-progress"))
  (reset! crafting-items (load-data "crafting-item"))
  (reset! weapons (load-data "weapon"))
  (reset! armours (load-data "armour"))
  (reset! enchants (load-data "enchant"))
  (reset! rings (load-data "ring"))
  (reset! consumables (load-data "consumable"))
  (reset! miscreations (load-data "miscreation"))
  (reset! monsters (load-data "monster"))
  (reset! uniques (load-data "unique"))
  (reset! character-enchants (load-data "character-enchant"))
  (reset! positive-encounters (load-data "positive-encounter"))
  (reset! riddles (load-data "riddle"))
  "Done")

(reload!)

(defn- relic-comparator [x y]
  (compare (relic-key-order x) (relic-key-order y)))

(defn override-relics! [new-relics]
  (reset! relics (mapv #(into (sorted-map-by relic-comparator) %) new-relics))
  (write-data! relics "relic"))

(defn override-prayer-progress! [new-progress]
  (reset! prayer-progressions new-progress)
  (write-data! prayer-progressions "prayer-progress"))

(defn override-riddles! [new-riddles]
  (reset! riddles new-riddles)
  (write-data! riddles "riddle"))
