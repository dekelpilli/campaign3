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

(defn- get-minimum-enchants [suit-tags num-mods {:keys [type base]}]
  (let [enchant-sampler (->> (e/valid-enchants base type)
                             (filterv (fn [{:keys [tags]}]
                                        (-> (set/intersection tags suit-tags)
                                            seq)))
                             u/weighted-sampler)]
    (repeatedly num-mods (comp u/fill-randoms enchant-sampler))))

(defn add-enchants []
  (u/when-let* [suit-tags (p/>>item "What Suit exceeded the minimum?" suit-tags)
                num-mods (some-> (p/>>input "How any mods to add?") parse-long)
                base (mundanes/choose-base)]
    (get-minimum-enchants suit-tags num-mods base)))

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
