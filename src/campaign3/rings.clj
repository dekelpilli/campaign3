(ns campaign3.rings
  (:require (campaign3
              [db :as db]
              [prompting :as p]
              [randoms :as randoms]
              [util :as u])
            [randy.core :as r]))

(def all-rings (->> (db/load-all :rings)
                    (map #(update % :randoms randoms/randoms->fn))))

(defn new-rings [n]
  (r/sample-without-replacement n all-rings))

(defn sacrifice []
  (when-let [sacrificed-rings (-> (p/>>input "Which rings are being sacrificed?"
                                             (map :name all-rings)
                                             :completer :comma-separated)
                                  not-empty)]
    (let [sacrificials-used (or (some-> (p/>>input "How many Sacrificial Orbs were used in this ring sacrifice?")
                                        parse-long)
                                0)
          remaining-rings (remove (comp (set sacrificed-rings) :name) all-rings)
          num-options (-> (count sacrificed-rings)
                          (* (inc sacrificials-used))
                          (min (count remaining-rings)))]
      (->> remaining-rings
           (r/sample-without-replacement num-options)
           (map u/fill-randoms)))))
