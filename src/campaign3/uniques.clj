(ns campaign3.uniques
  (:require [campaign3.db :as db]
            [randy.core :as r]))

(def uniques (db/load-all :uniques))

(defn new []
  (r/sample uniques))
