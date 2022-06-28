(ns campaign3.rings
  (:require (campaign3
              [db :as db]
              [prompting :as p]
              [randoms :as randoms]
              [util :as u])
            [randy.core :as r]))

(def all-rings (->> (db/load-all :rings)
                    (mapv #(update % :randoms randoms/randoms->fn))))
(def synergy-rings (filterv :synergy all-rings))
(def regular-rings (filterv (complement :synergy) all-rings))

(defn new-non-synergy []
  (->> regular-rings
       (r/sample)
       (u/fill-randoms)))

(defn new-synergy []
  (->> synergy-rings
       (r/sample)
       (u/fill-randoms)))

(defn >>sacrifice []
  (when-let [sacrificed-rings (-> (p/>>input "Which rings are being sacrificed?"
                                             (map :name all-rings)
                                             :completer :comma-separated)
                                  (not-empty))]
    (let [catalysts-used (or (some-> (p/>>input "How many catalysts were used in this ring sacrifice?")
                                     (parse-long))
                             0)
          remaining-rings (remove (comp (set sacrificed-rings) :name) all-rings)
          num-options (-> (count sacrificed-rings)
                          (* (inc catalysts-used))
                          (min (count remaining-rings)))]
      (->> remaining-rings
           (r/sample-without-replacement num-options)
           (map u/fill-randoms)))))
