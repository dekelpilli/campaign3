(ns campaign3.tarot
  (:require (campaign3
              [enchants :as e]
              [db :as db]
              [mundanes :as mundanes]
              [prompting :as p]
              [util :as u])
            [clojure.set :as set]))

(def suit-tags {:swords    #{"accuracy" "damage"}
                :wands     #{"magic" "critical"}
                :cups      #{"survivability" "control"}
                :pentacles #{"utility" "wealth"}})

(def cards (->> (db/load-all :tarot-cards)
                (u/assoc-by :name)))

(defn lookup []
  (-> (p/>>item "What is the Tarot card?" cards)
      :effect))

(comment
  "Probabilities of count of court cards"
  "4 cards, only ace of x"
  {0 0.15036, 1 0.38744, 2 0.3337, 3 0.11511, 4 0.01339}

  "6 cards, added numerics to deck"
  {0 0.2399, 1 0.40295, 2 0.25985, 3 0.08296, 4 0.01301, 5 0.00129, 6 4.0E-5})

(defn- get-minimum-enchants [suit-tags num-mods {:keys [type base]}]
  (let [enchant-sampler (->> (e/valid-enchants base type)
                             (filterv (fn [{:keys [tags]}]
                                        (-> (set/intersection tags suit-tags)
                                            seq)))
                             u/weighted-sampler)]
    (repeatedly num-mods (comp u/fill-randoms enchant-sampler))))

(defn add-enchants []
  (u/when-let* [suits (-> (p/>>distinct-items "What Suit exceeded the minimum?" (keys suit-tags))
                          not-empty)
                num-mods (-> (into {} (comp
                                        (map (fn [suit] [suit
                                                         (or (some-> (p/>>input (format "How any mods to add for '%s'?"
                                                                                        (name suit)))
                                                                     parse-long)
                                                             0)]))
                                        (filter second))
                                   suits)
                             not-empty)
                base (mundanes/choose-base)]
    (mapcat (fn [[suit amount]]
              (get-minimum-enchants (get suit-tags suit) amount base))
            num-mods)))

(defn new-blank-relic! []
  (u/when-let* [suits (-> (p/>>distinct-items "What Suits were used in this turn in?" (keys suit-tags))
                          seq)
                suit-tag-freqs (reduce
                                 (fn [acc suit]
                                   (if-let [amount (some-> (p/>>input (str "How many '" suit "' suit cards?")) parse-long)]
                                     (assoc acc (get suit-tags suit) amount)
                                     (reduced nil)))
                                 {}
                                 suits)
                relic-name (p/>>input "What is the relic's name?")
                {:keys [base type] :as relic-base} (mundanes/choose-base)]
    (let [starting-mods (-> (mapcat (fn [[tags num]]
                                      (get-minimum-enchants tags num relic-base))
                                    suit-tag-freqs)
                            vec)]
      (db/execute! {:insert-into :relics
                    :values      [{:name      relic-name
                                   :found     true
                                   :base-type type
                                   :base      (:name base)
                                   :start     (u/jsonb-lift starting-mods)
                                   :mods      (u/jsonb-lift [])
                                   :levels    (u/jsonb-lift {})}]})
      starting-mods)))
