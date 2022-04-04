(ns campaign3.parser
  (:require [campaign3.state :as state]
            [campaign3.util :as u]
            [jsonista.core :as json])
  (:import (java.io File)))

(def ^String dir "loot/data/5et/")

(defn flatten-maps [k maps]
  (let [explode (fn [m key] (map #(assoc m key %) (m key)))]
    (-> (map #(explode % k) maps)
        (flatten))))

(defn find-cr [{:keys [cr]}]
  (u/->num (if (map? cr) (:cr cr) cr)))

(defn load-monsters []
  (let [files (->> dir
                   (File.)
                   (file-seq)
                   (remove #(.isDirectory %)))
        mons (->> files
                  (map slurp)
                  (map #(json/read-value % json/keyword-keys-object-mapper))
                  (map :monster)
                  (flatten)
                  (remove #(contains? % :_copy))
                  (group-by find-cr)
                  (filter first)
                  (map (fn [[k v]] [k
                                    (mapv #(select-keys % [:name :source :page :type]) v)]))
                  (into (sorted-map)))]
    (state/write-data! (delay mons) "monster")))
