(ns campaign3.prep
  (:require [campaign3.prompting :as p]
            [campaign3.util :as u]
            [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :as combo]
            [randy.core :as r]))

(defn- new-room-dimensions []
  (vec (repeatedly 2 #(+ 4 (rand-int 6)))))

(defn- new-room-contents []
  ((r/sample [#(format "Easy: %s mobs" (+ 2 (rand-int 5)))
              #(format "Medium: %s mobs" (+ 2 (rand-int 4)))
              #(format "Hard: %s mobs" (+ 4 (rand-int 3)))
              (constantly "Hard: 2 mobs")
              (constantly "Puzzle/trap")])))

(defn- unordered-selections [coll n]
  (sequence (comp (map sort) (distinct))
            (combo/selections coll n)))


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

;https://www.gmbinder.com/share/-N4m46K77hpMVnh7upYa
(def ^:private encounter-difficulties [{:tier "Mild" :multiplier 0.40 :cost 2}
                                       {:tier "Bruising" :multiplier 0.60 :cost 4}
                                       {:tier "Bloody" :multiplier 0.75 :cost 6}
                                       {:tier "Brutal" :multiplier 0.90 :cost 8}
                                       {:tier "Oppressive" :multiplier 1.00 :cost 10}
                                       {:tier "Overwhelming" :multiplier 1.10 :cost 13}
                                       {:tier "Crushing" :multiplier 1.30 :cost 17}
                                       {:tier "Devastating" :multiplier 1.60 :cost 25}
                                       {:tier "Impossible" :multiplier 2.25 :cost 50}])
(def ^:private players 3)
(def ^:private max-enemies 6)

(def ^:private basic-level-power [11 14 18 23 32 35 41 44 49 53 62 68 71 74 82 84 103 119 131 141])
(def ^:private basic-cr-power {0  1 1/8 5 1/4 10 1/2 16 1 22 2 28 3 37 4 48 5 60 6 65 7 70 8 85 9 85
                               10 95 11 105 12 115 13 120 14 125 15 130 16 140 17 150 18 160 19 165
                               20 180 21 200 22 225 23 250 24 275 25 300 26 325 27 350 28 375 29 400
                               30 425})

(defn- basic-legal-cr-set? [crs lower upper]
  (let [total (transduce (map basic-cr-power) + crs)]
    (<= lower total upper)))

(defn basic-cr2-encounter []
  (u/when-let* [level (some-> (p/>>input "What level are the players?") parse-long)
                min-cr (p/>>item "What is the minimum CR of monsters in this encounter?"
                                 (keys basic-cr-power))
                {:keys [multiplier]} (p/>>item "What is the target difficulty of this encounter?"
                                               (u/assoc-by :tier encounter-difficulties))]
    (let [player-power (->> (dec level)
                            (nth basic-level-power)
                            (* players))
          target-monster-power (* multiplier player-power)
          target-monster-power-lower (* 0.95 target-monster-power)
          target-monster-power-upper (* 1.05 target-monster-power)
          crs (keep (fn [[cr power]]
                      (when (and (>= cr min-cr) (<= power player-power))
                        cr))
                    basic-cr-power)]
      (reduce (fn [acc n]
                (if-let [legal-cr-combos (->> (unordered-selections crs n)
                                              (filterv #(basic-legal-cr-set? % target-monster-power-lower target-monster-power-upper))
                                              not-empty)]
                  (assoc acc n legal-cr-combos)
                  (if (empty? acc)
                    acc
                    (reduced acc))))
              {}
              (range 1 (inc max-enemies))))))

(def ^:private adv-level-power [2 7 10 13 18 20 22 23 24 25 28 29 30 30 32 32 35 37 39 40])
(def ^:private adv-player-power [11 11 12 13 14 15 16 17 18 20 21 22 24 26 28 30 32 34 36 39 42 45 48 51 55 59 63 67 72 77 83 89 95 102 109 117 125 134 143 154 165 176 189 202 216 232 248])
(def ^:private adv-cr->power-tiers {0 [1 1 0 0] 1/8 [4 3 3 2] 1/4 [10 6 5 4] 1/2 [16 12 7 5] 1 [22 17 15 8] 2 [28 23 19 14] 3 [37 30 25 19] 4 [48 38 32 24] 5 [70 60 45 40] 6 [80 65 50 40] 7 [90 70 55 45] 8 [105 85 70 55] 9 [110 85 70 55] 10 [115 95 75 60] 11 [140 130 105 85] 12 [150 140 115 90] 13 [160 150 120 95] 14 [165 155 125 100] 15 [175 165 130 105] 16 [185 175 140 110] 17 [250 200 190 150] 18 [260 210 200 160] 19 [280 220 210 170] 20 [300 240 230 180] 21 [400 350 275 250] 22 [450 375 300 275] 23 [500 425 325 325] 24 [550 450 375 350] 25 [600 500 400 375] 26 [650 525 425 400] 27 [725 600 475 450] 28 [775 625 500 475] 29 [775 650 525 475] 30 [850 725 575 525]})

(defn- adv-legal-cr-set? [crs cr->power lower upper]
  (let [total (transduce (map cr->power) + crs)]
    (<= lower total upper)))

(defn advanced-cr2-encounter
  "Currently skips steps 1D and 1E"
  []
  (u/when-let* [level (some-> (p/>>input "What level are the players?") parse-long)
                min-cr (p/>>item "What is the minimum CR of monsters in this encounter?"
                                 (keys adv-cr->power-tiers))
                {:keys [multiplier]} (p/>>item "What is the target difficulty of this encounter?" encounter-difficulties :sorted? false)]
    (let [player-power (->> (dec level)
                            (nth adv-level-power)
                            (nth adv-player-power)
                            (* players))
          player-tier (condp >= level
                        4 1
                        10 2
                        16 3
                        20 4)
          target-monster-power (* multiplier player-power)
          target-monster-power-lower (* 0.95 target-monster-power)
          target-monster-power-upper (* 1.05 target-monster-power)
          cr->power (update-vals adv-cr->power-tiers #(nth % (dec player-tier)))
          crs (keep (fn [[cr power]]
                      (when (and (>= cr min-cr) (<= power player-power))
                        cr))
                    cr->power)]
      (reduce (fn [acc n]
                (if-let [legal-cr-combos (->> (unordered-selections crs n)
                                              (filterv #(adv-legal-cr-set?
                                                          %
                                                          cr->power
                                                          target-monster-power-lower
                                                          target-monster-power-upper))
                                              not-empty)]
                  (assoc acc n legal-cr-combos)
                  (if (empty? acc)
                    acc
                    (reduced acc))))
              {}
              (range 1 (inc max-enemies))))))
