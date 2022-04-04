(ns campaign3.dice
  (:require [org.fversnel.dnddice.core :as d]))

(defn parse [s]
  (try
    (d/parse s)
    (catch Exception _
      nil)))

(defn roll [r]
  (update (d/roll r) :die-rolls seq))
