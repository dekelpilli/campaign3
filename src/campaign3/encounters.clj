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

(def ^:private activities [:befriend-creature :busk :chronicle :entertain :force-march :gather-components
                           :gossip :harvest :pray :rob :scout])
(def ^:private weather-dc-mods ;TODO
  {:rain         {:busk 1 :befriend-creature -1}
   :clear        {:chronicle -1}
   :frigid       {}
   :sweltering   {}
   :snow         {}
   :hail         {:busk 2}
   :fog          {:chronicle 2}
   :overcast     {}
   :sandstorm    {:all               3
                  :force-march       2
                  :befriend-creature -1
                  :pray              0
                  :rob               -1}
   :acid-rain    {:all               4
                  :pray              0
                  :befriend-creature -2
                  :force-march       2}
   :thunderstorm {}})
(def ^:private weather-fns
  (-> {:rain         {:rain         15
                      :frigid       8
                      :clear        6
                      :snow         2
                      :hail         2
                      :fog          4
                      :overcast     10
                      :thunderstorm 6}
       :clear        {:rain         8
                      :clear        15
                      :frigid       2
                      :sweltering   12
                      :fog          1
                      :overcast     10
                      :sandstorm    1
                      :thunderstorm 1}
       :snow         {:rain         4
                      :clear        2
                      :frigid       6
                      :snow         6
                      :hail         5
                      :fog          1
                      :overcast     2
                      :thunderstorm 1}
       :hail         {:rain         10
                      :clear        2
                      :frigid       10
                      :snow         3
                      :hail         8
                      :fog          3
                      :overcast     6
                      :thunderstorm 6}
       :fog          {:rain         8
                      :clear        2
                      :frigid       8
                      :snow         1
                      :hail         2
                      :fog          8
                      :overcast     10
                      :thunderstorm 3}
       :overcast     {:rain         10
                      :clear        6
                      :sweltering   2
                      :frigid       8
                      :hail         1
                      :fog          7
                      :overcast     15
                      :thunderstorm 2}
       :sandstorm    {:rain       1
                      :clear      6
                      :sweltering 6
                      :frigid     1
                      :fog        1
                      :overcast   2
                      :sandstorm  3
                      :acid-rain  1}
       :acid-rain    {:rain       1
                      :sweltering 4
                      :frigid     1
                      :clear      2
                      :overcast   4
                      :sandstorm  2
                      :acid-rain  3}
       :thunderstorm {:rain         15
                      :clear        4
                      :sweltering   1
                      :frigid       6
                      :snow         2
                      :hail         4
                      :fog          2
                      :overcast     8
                      :thunderstorm 10}}
      (update-vals r/alias-method-sampler)))

(def ^:private default-travel-weightings {nil       75
                                          :positive 5
                                          :random   20})

(def positive-encounters (db/load-all :positive-encounters))

(defn- add-encounter! [type]
  (u/record! (str "encounter" type) 1)
  type)

(defn activity-mod []
  (u/when-let* [mods (p/>>item "What's the weather?" weather-dc-mods)
                activities (p/>>item "What are activities are being performed?" activities)]
    (reduce (fn [acc activity] (assoc acc activity (get mods activity (get mod :all 0)))) {} activities)))

(defn pass-time [days]
  (when-let [initial-weather (p/>>item "What was the weather yesterday?" (keys weather-fns))]
    (u/record! "days:other" days)
    (reduce (fn [weather _]
              ((get weather-fns weather)))
            initial-weather
            (range days))))

(defn travel [days]
  (when-let [initial-weather-fn (p/>>item "What was the weather yesterday?" weather-fns)]
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
      (loop [acc (sorted-map)
             previous-weather-fn initial-weather-fn
             [day & days] (range 1 (inc days))]
        (let [encounter (r/weighted-sample random-encounter-prob)
              weather (previous-weather-fn)
              acc (assoc acc day {:encounter encounter :weather weather})]
          (when encounter
            (add-encounter! encounter))
          (if (seq days)
            (recur acc (get weather-fns weather) days)
            acc))))))

(defn- calculate-loot [difficulty investigations]
  (let [extra-loot-sum (transduce (map (fn [s] (- (parse-long s) extra-loot-threshold))) + 0 investigations)
        dungeon? (when-not (#{:easy :medium} difficulty)
                   (p/>>item "In a dungeon?" [true false] :none-opt? false))
        base-loot (case difficulty
                    (:mild :bruising) 0
                    :bloody (if dungeon? 1 0)
                    :brutal (if dungeon? 2 1)
                    (:boss :oppressive) (if dungeon? 4 2)
                    :overwhelming (if dungeon? 5 3)
                    :crushing (if dungeon? 6 4)
                    :devastating (if dungeon? 8 5))]
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
    {:xp   (case difficulty
             :mild (+ 6 (rng/next-int r/default-rng 2))
             :bruising (+ 7 (rng/next-int r/default-rng 2))
             :bloody (+ 8 (rng/next-int r/default-rng 2))
             :brutal (+ 10 (rng/next-int r/default-rng 3))
             :oppressive (+ 13 (rng/next-int r/default-rng 3))
             (:boss :overwhelming) (+ 14 (rng/next-int r/default-rng 3))
             :crushing (+ 16 (rng/next-int r/default-rng 3))
             :devastating (+ 18 (rng/next-int r/default-rng 3)))
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
