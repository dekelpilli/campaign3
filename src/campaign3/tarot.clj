(ns campaign3.tarot
  (:require (campaign3
              [enchants :as e]
              [db :as db]
              [mundanes :as mundanes]
              [prompting :as p]
              [util :as u])
            [clojure.set :as set]))

(def cards (->> (db/load-all :tarot-cards)
                (u/assoc-by :name)))

(defn lookup []
  (-> (p/>>item "What is the Tarot card?" cards)
      :effect))

(defn add-enchants []
  (u/when-let* [suit-tags (p/>>item "What Suit exceeded the minimum?" {:swords    #{"accuracy" "damage"}
                                                                       :wands     #{"magic" "critical"}
                                                                       :cups      #{"survivability" "control"}
                                                                       :pentacles #{"utility" "wealth"}})
                num-mods (some-> (p/>>input "How any mods to add?") parse-long)
                {:keys [type base]} (mundanes/choose-base)]
    (let [enchant-sampler (->> (e/valid-enchants base type)
                               (filterv (fn [{:keys [tags]}]
                                          (-> (set/intersection tags suit-tags)
                                              seq)))
                               u/weighted-sampler)]
      (repeatedly num-mods (comp u/fill-randoms enchant-sampler)))))
