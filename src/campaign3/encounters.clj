(ns campaign3.encounters
  (:require (campaign3
              [db :as db]
              [prompting :as p]
              [util :as u])
            [clojure.string :as str]
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
