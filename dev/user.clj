(ns user
  (:require [clojure.tools.namespace.repl :as repl]
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

(def reqs '[[campaign3.util u]
            [campaign3.prompting p]
            [campaign3.relics relics]
            [campaign3.mundanes mundane]
            [campaign3.enchants e]
            [campaign3.crafting crafting]
            [campaign3.consumables consumables]
            [campaign3.prayers prayers]
            [campaign3.encounters encounter]
            [campaign3.dice dice]
            [campaign3.rings rings]
            [campaign3.uniques unique]
            [campaign3.riddle riddle]
            [campaign3.core core]])

(defmacro realias! []
  (cons 'do
        (reduce (fn [l [ns-sym alias-sym]]
                  (cons (list `ns-unalias `*ns* `'~alias-sym)
                        (cons
                          (list `alias `'~alias-sym `'~ns-sym)
                          l)))
                '()
                reqs)))

(hawk/watch! [{:paths   ["src"]
               :handler (fn [ctx e]
                          (future (repl/refresh)))}])
