(ns campaign3.encounter
  (:require [campaign3.util :as util]
            [campaign3.state :refer [positive-encounters]]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(def ^:private extra-loot-threshold 13)
(def ^:private extra-loot-step 6)
(def ^:private races ["Aarakocra" "Aasimar" "Bugbear" "Centaur" "Changeling" "Dragonborn" "Dwarf" "Elf" "Firbolg"
                      "Genasi" "Gith" "Gnome" "Goblin" "Goliath" "Half-Elf" "Half-Orc" "Halfling" "Hobgoblin" "Human"
                      "Kalashtar" "Kenku" "Kobold" "Lizardfolk" "Loxodon" "Minotaur" "Orc" "Satyr" "Shifter" "Tabaxi"
                      "Tiefling" "Tortle" "Triton" "Vedalken" "Yuan-Ti Pureblood"])
(def ^:private sexes ["female" "male"])
(def ^:private had-random? (atom false))

(defn travel [^long days]
  (->> (range 1 (inc days))
       (map (fn [i]
              [i
               (when (util/occurred? (if @had-random? 0.10 0.25))
                 (if (util/occurred? 0.2)
                   :positive
                   (do
                     (reset! had-random? true)
                     :random)))]))
       (into (sorted-map))))
(defn &travel
  ([]
   (println "How many days?")
   (some-> (util/&num) (travel))))

(defn- add-loot [extra-loot-factor base-loot]
  (let [extra-loot? (pos? extra-loot-factor)
        loot (if extra-loot?
               (concat base-loot (repeat (-> extra-loot-factor (/ extra-loot-step) (int)) "1d16"))
               base-loot)
        remainder (mod extra-loot-factor extra-loot-step)
        remainder-above-half-step? (-> remainder
                                       (> (int (/ extra-loot-step 2))))
        bonus-loot (match [extra-loot? remainder remainder-above-half-step?]
                          [false _ _] []
                          [_ 0 _] []
                          [true _ true] ["2d8"]
                          [true _ false] ["1d12"])]
    (concat loot bonus-loot)))

(defn- calculate-loot [difficulty investigations]
  (let [investigations (map #(Integer/parseInt %) investigations)
        sum (reduce + investigations)
        extra-loot-minimum (* extra-loot-threshold (count investigations))
        extra-loot-factor (- sum extra-loot-minimum)
        base-loot (case difficulty
                    :easy ["2d8"]
                    :medium ["2d8" "1d12"]
                    :hard ["1d16" "1d16"]
                    :deadly ["1d16" "1d16" "1d12"])]
    (->> base-loot
         (add-loot extra-loot-factor)
         (frequencies)
         (sort-by {"1d16" 1 "2d8" 2 "1d12" 3}))))

(defn rewards [difficulty investigations]
  (when (and (keyword? difficulty) (vector? investigations))
    {:xp   (case difficulty
             :easy (+ 6 (rand-int 2))
             :medium (+ 8 (rand-int 3))
             :hard (+ 11 (rand-int 3))
             :deadly (+ 13 (rand-int 4)))
     :loot (calculate-loot difficulty investigations)}))
(defn &rewards
  ([]
   (let [difficulties (util/display-pairs
                        (util/make-options [:easy :medium :hard :deadly]))
         difficulty (difficulties (util/&num))
         investigations (when difficulty
                          (println "List investigations: ")
                          (read-line))
         investigations (when investigations (str/split investigations #","))]
     (rewards difficulty investigations))))

(defn new-positive []
  {:race      (rand-nth races)
   :sex       (rand-nth sexes)
   :encounter (rand-nth @positive-encounters)})

(defn- new-room-dimensions []
  (vec (repeatedly 2 #(+ 4 (rand-int 6)))))

(defn- new-room-contents []
  ((rand-nth [#(format "Easy: %s mobs" (+ 2 (rand-int 5)))
              #(format "Medium: %s mobs" (+ 2 (rand-int 4)))
              #(format "Hard: %s mobs" (+ 4 (rand-int 3)))
              (constantly "Hard: 2 mobs")
              (constantly "Puzzle/trap")])))

(defn new-dungeon []
  (loop [remaining 3
         rooms []]
    (if (pos? remaining)
      (let [[x y] (new-room-dimensions)
            new-room {:x x :y y :contents (new-room-contents)}
            rooms (conj rooms new-room)]
        (match [x y]
               [9 9] (recur (inc remaining) rooms)
               [9 _] (recur remaining rooms)
               [_ 9] (recur remaining rooms)
               [_ _] (recur (dec remaining) rooms)))
      (as-> (new-room-dimensions) $
            (conj rooms {:x (first $) :y (second $) :contents "Boss"})))))
