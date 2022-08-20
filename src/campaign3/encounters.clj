(ns campaign3.encounters
  (:require (campaign3
              [db :as db]
              [prompting :as p]
              [util :as u])
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [randy.core :as r]
            [randy.rng :as rng]))

(def ^:private extra-loot-threshold 13)
(def ^:private extra-loot-step 2)
(def ^:private races ["Aarakocra" "Aasimar" "Bugbear" "Centaur" "Changeling" "Dragonborn" "Dwarf" "Elf" "Firbolg"
                      "Genasi" "Gith" "Gnome" "Goblin" "Goliath" "Half-Elf" "Half-Orc" "Halfling" "Harengon" "Hobgoblin"
                      "Human" "Kalashtar" "Kenku" "Kobold" "Leonin" "Lizardfolk" "Loxodon" "Minotaur" "Orc" "Owlin"
                      "Satyr" "Shifter" "Tabaxi" "Tiefling" "Tortle" "Triton" "Vedalken" "Yuan-Ti Pureblood"])
(def ^:private sexes ["female" "male"])

(def positive-encounters (db/load-all :positive-encounters))

(defn- add-encounter! [type]
  (u/record! (str "encounter" type) 1)
  type)

(defn pass-time [days]
  (u/record! "days:other" days))

(def default-travel-weightings {nil       75
                                :positive 5
                                :random   20})

(defn travel [days]
  (u/record! "days:travel" days)
  (let [had-random? (when (bound? #'u/session)
                      (-> (db/execute! {:select [[[:> :amount 0] :had-random]]
                                        :from   [:analytics]
                                        :where  [:and
                                                 [:= :session u/session]
                                                 [:= :type "encounter:random"]]})
                          first
                          :had-random))
        random-encounter-prob (cond-> default-travel-weightings
                                      had-random? (update :random #(- % 10)))]
    (into (sorted-map)
          (map (fn [i]
                 (let [encounter (r/weighted-sample random-encounter-prob)]
                   (when encounter
                     (add-encounter! encounter))
                   [i encounter])))
          (range 1 (inc days)))))

(defn- calculate-loot [difficulty investigations]
  (let [extra-loot-sum (transduce (map (fn [s] (- (parse-long s) extra-loot-threshold))) + 0 investigations)
        dungeon? (when-not (#{:easy :medium} difficulty)
                   (p/>>item "In a dungeon?" [true false] :none-opt? false))
        base-loot (case difficulty
                    (:easy :medium) 0
                    :hard (if dungeon? 1 0)
                    :deadly (if dungeon? 2 1)
                    :boss (if dungeon? 4 2))]
    (->> (count investigations)
         (* extra-loot-step)
         (/ extra-loot-sum)
         int
         (max 0)
         (+ base-loot))))

(defn rewards []
  (u/when-let* [difficulty (p/>>item "Difficulty:" [:easy :medium :hard :deadly :boss] :sorted? false)
                investigations (some-> (p/>>input "List investigations:")
                                       (str/split #","))]
    ;TODO XP too high?
    {:xp   (case difficulty
             :easy (+ 6 (rng/next-int r/default-rng 2))
             :medium (+ 8 (rng/next-int r/default-rng 3))
             :hard (+ 11 (rng/next-int r/default-rng 3))
             :deadly (+ 13 (rng/next-int r/default-rng 4))
             :boss (+ 15 (rng/next-int r/default-rng 4)))
     :loot (calculate-loot difficulty investigations)}))

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

;https://www.gmbinder.com/share/-N4m46K77hpMVnh7upYa
(def level-power [11 14 18 23 32 35 41 44 49 53 62 68 71 74 82 84 103 119 131 141])
(def cr-power {0  1 1/8 5 1/4 10 1/2 16 1 22 2 28 3 37 4 48 5 60 6 65 7 70 8 85 9 85
               10 95 11 105 12 115 13 120 14 125 15 130 16 140 17 150 18 160 19 165
               20 180 21 200 22 225 23 250 24 275 25 300 26 325 27 350 28 375 29 400
               30 425}) ;CR8=CR9?
(def encounter-difficulties [{:tier "Mild" :multiplier 0.40 :cost 2}
                             {:tier "Bruising" :multiplier 0.60 :cost 4}
                             {:tier "Bloody" :multiplier 0.75 :cost 6}
                             {:tier "Brutal" :multiplier 0.90 :cost 8}
                             {:tier "Oppressive" :multiplier 1.00 :cost 10}
                             {:tier "Overwhelming" :multiplier 1.10 :cost 13}
                             {:tier "Crushing" :multiplier 1.30 :cost 17}
                             {:tier "Devastating" :multiplier 1.60 :cost 25}
                             {:tier "Impossible" :multiplier 2.25 :cost 50}])
(def players 3)
(def max-enemies 6)

(defn- legal-cr-set? [crs lower upper]
  (let [total (transduce (map cr-power) + crs)]
    (<= lower total upper)))

(defn- unordered-selections [coll n]
  (sequence (comp (map sort) (distinct))
            (combo/selections coll n)))

(defn generate-encounter []
  (u/when-let* [level (some-> (p/>>input "What level are the players?") parse-long)
                min-cr (p/>>item "What is the minimum CR of monsters in this encounter?"
                                 (keys cr-power))
                {:keys [multiplier]} (p/>>item "What is the target difficulty of this encounter?"
                                               (u/assoc-by :tier encounter-difficulties))]
    (let [player-power (->> (dec level)
                            (nth level-power)
                            (* players))
          target-monster-power (* multiplier player-power)
          target-monster-power-lower (* 0.95 target-monster-power)
          target-monster-power-upper (* 1.05 target-monster-power)
          crs (keep (fn [[cr power]]
                      (when (and (>= cr min-cr) (<= power player-power))
                        cr))
                    cr-power)]
      (reduce (fn [acc n]
                (if-let [legal-cr-combos (->> (unordered-selections crs n)
                                              (filterv #(legal-cr-set? % target-monster-power-lower target-monster-power-upper))
                                              not-empty)]
                  (assoc acc n legal-cr-combos)
                  (if (empty? acc)
                    acc
                    (reduced acc))))
              {}
              (range 1 (inc max-enemies))))))
