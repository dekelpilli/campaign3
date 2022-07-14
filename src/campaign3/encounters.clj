(ns campaign3.encounters
  (:require (campaign3
              [db :as db]
              [prompting :as p]
              [util :as u])
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [randy.core :as r]))

(def ^:private extra-loot-threshold 13)
(def ^:private extra-loot-step 6)
(def ^:private races ["Aarakocra" "Aasimar" "Bugbear" "Centaur" "Changeling" "Dragonborn" "Dwarf" "Elf" "Firbolg"
                      "Genasi" "Gith" "Gnome" "Goblin" "Goliath" "Half-Elf" "Half-Orc" "Halfling" "Hobgoblin" "Human"
                      "Kalashtar" "Kenku" "Kobold" "Lizardfolk" "Loxodon" "Minotaur" "Orc" "Satyr" "Shifter" "Tabaxi"
                      "Tiefling" "Tortle" "Triton" "Vedalken" "Yuan-Ti Pureblood"])
(def ^:private sexes ["female" "male"])

(def positive-encounters (db/load-all :positive-encounters))

(defn- add-encounter! [type]
  (when u/session
    (db/execute! {:update :analytics
                  :set    {:amount [:+ :amount 1]}
                  :where  [:and
                           [:= :session u/session]
                           [:= :type (str "encounter" :positive)]]}))
  type)

(defn travel [^long days]
  (let [had-random? (when (bound? #'u/session)
                      (-> (db/execute! {:select [:amount]
                                        :from   [:analytics]
                                        :where  [:and
                                                 [:= :session u/session]
                                                 [:= :type "encounter:random"]]})
                          (first)
                          (:amount)
                          (> 0)))
        random-encounter-prob (if had-random? 0.10 0.25)] ;TODO adjust numbers once world map is done
    (into (sorted-map)
          (map (fn [i]
                 [i
                  (when (u/occurred? random-encounter-prob)
                    (add-encounter! (if (u/occurred? 0.2) :positive :random)))]))
          (range 1 (inc days)))))

(defn- add-loot [extra-loot-factor base-loot]
  (let [extra-loot? (pos? extra-loot-factor)
        loot (if extra-loot?
               (concat base-loot (repeat (-> extra-loot-factor (/ extra-loot-step) int) "1d16"))
               base-loot)
        remainder (mod extra-loot-factor extra-loot-step)
        remainder-above-half-step? (> remainder (int (/ extra-loot-step 2)))
        bonus-loot (match [extra-loot? remainder remainder-above-half-step?]
                          [false _ _] []
                          [_ 0 _] []
                          [true _ true] ["2d8"]
                          [true _ false] ["1d12"])]
    (concat loot bonus-loot)))

(defn- calculate-loot [difficulty investigations]
  (let [sum (transduce (map parse-long) + 0 investigations)
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

(defn rewards [] ;TODO adjust for 1d12 format, reduce values, account for one-off vs in-dungeon fights? add boss tier?
  (when-let [difficulty (p/>>item [:easy :medium :hard :deadly])]
    (when-let [investigations (some-> (p/>>input "List investigations:")
                                      (str/split #","))]
      {:xp   (case difficulty
               :easy (+ 6 (rand-int 2))
               :medium (+ 8 (rand-int 3))
               :hard (+ 11 (rand-int 3))
               :deadly (+ 13 (rand-int 4)))
       :loot (calculate-loot difficulty investigations)})))

(defn new-positive []
  {:race      (r/sample races)
   :sex       (r/sample sexes)
   :encounter (r/sample positive-encounters)})

(defn- new-room-dimensions []
  (vec (repeatedly 2 #(+ 4 (rand-int 6)))))

(defn- new-room-contents []
  ((r/sample [#(format "Easy: %s mobs" (+ 2 (rand-int 5)))
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

;d100 + this for a positive random encounter, with result being the total points of a magic item?
; could also give nothing if they go above the number, or a low loot roll
(defn- num-char->num [c]
  (- (int c) 48))

(defn cheiro-sum [word maximum]
  (let [char-values {\a 1 \b 2 \c 3 \d 4 \e 5
                     \f 8 \g 3 \h 5 \i 1 \j 1
                     \k 2 \l 3 \m 4 \n 5 \o 7
                     \p 8 \q 1 \r 2 \s 3 \t 4
                     \u 6 \v 6 \w 6 \x 5 \y 1 \z 7}
        sum (transduce (map char-values) + 0 (str/lower-case word))]
    (loop [sum sum]
      (if (or (<= sum maximum) (< sum 10))
        sum
        (recur (transduce (map num-char->num) + 0 (str sum)))))))
