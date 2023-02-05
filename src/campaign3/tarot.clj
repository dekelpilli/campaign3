(ns campaign3.tarot
  (:require (campaign3
              [db :as db]
              [enchants :as e]
              [helmets :as helmets]
              [mundanes :as mundanes]
              [prompting :as p]
              [relics :as relics]
              [util :as u])
            [clojure.set :as set]
            [puget.printer :as puget]
            [randy.core :as r]))

(def ^:private suit-tags {:swords    #{"accuracy" "damage"}
                          :wands     #{"magic" "critical"}
                          :cups      #{"survivability" "control"}
                          :pentacles #{"utility" "wealth"}})

(def ^:private cards (->> (db/load-all :tarot-cards)
                          (u/assoc-by :name)))

(defn lookup-card []
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
    (repeatedly num-mods (comp e/prep-enchant enchant-sampler))))

(defn add-tarot-enchants! []
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
                             not-empty)]
    (let [{:keys [base base-type] :as relic} (relics/choose-found-relic)]
      (u/when-let* [base (if relic
                           (mundanes/name->base base-type base)
                           (mundanes/choose-base))
                    mods (mapcat (fn [[suit amount]]
                                   (get-minimum-enchants (get suit-tags suit) amount base))
                                 num-mods)]
        (if relic
          (do
            (-> relic
                (update :start into mods)
                (update :levels #(mapv (fn inject-relic-mods [level]
                                        (update level :existing into mods))
                                      %))
                relics/update-relic!)
            mods)
          mods)))))

(defn add-character-enchants []
  (u/when-let* [character-enchants (p/>>item "Character name:" helmets/character-enchants)
                amount (p/>>item "How many enchants should be added to the item?" (range 1 (inc (count character-enchants))))]
    (r/sample-without-replacement amount character-enchants)))

(defn new-blank-relic! []
  (u/when-let* [suits (-> (p/>>distinct-items "What Suits were used in this turn in?" (keys suit-tags))
                          seq)
                suit-tag-freqs (reduce
                                 (fn [acc suit]
                                   (if-let [amount (some-> (p/>>input (str "How many '" (name suit) "' suit cards?")) parse-long)]
                                     (assoc acc (get suit-tags suit) amount)
                                     (reduced nil)))
                                 {}
                                 suits)
                {:keys [base type] :as relic-base} (mundanes/choose-base)]
    (let [starting-mods (-> (mapcat (fn [[tags num]]
                                      (get-minimum-enchants tags num relic-base))
                                    suit-tag-freqs)
                            vec)]
      (puget/cprint starting-mods)
      (when-let [relic-name (p/>>input "What is the relic's name?")]
        (db/execute! {:insert-into :relics
                      :values      [{:name      relic-name
                                     :found     true
                                     :base-type type
                                     :base      (:name base)
                                     :start     (u/jsonb-lift starting-mods)
                                     :mods      (u/jsonb-lift [])
                                     :levels    (u/jsonb-lift [])}]})
        starting-mods))))
