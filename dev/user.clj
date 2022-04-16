(ns user
  (:require [clojure.tools.namespace.repl :as repl]
            [clojure.pprint :refer [pprint pp]]
            [hawk.core :as hawk]
            [campaign3
             [util :as u]
             [prompting :as p]
             [relics :as relics]
             [mundanes :as mundane]
             [enchants :as e]
             [crafting :as crafting]
             [consumables :as consumables]
             [prayers :as prayers]
             [encounters :as encounter]
             [dice :as dice]
             [rings :as rings]
             [uniques :as unique]
             [riddle :as riddle]
             [core :as core]]))

(defmacro realias! []
  (cons 'do
        (reduce (fn [l [alias-sym ns-sym]]
                  (->> l
                       (cons (list `alias `'~alias-sym `'~ns-sym))
                       (cons (list `ns-unalias `*ns* `'~alias-sym))))
                '()
                (update-vals (ns-aliases *ns*) (comp symbol str)))))

(hawk/watch! [{:paths   ["src"]
               :handler (fn [ctx e]
                          (future (repl/refresh)))}])
