(ns campaign3.dsls
  (:require [campaign3.dice :as dice]
            [clojure.walk :as walk]))

(defn- disadv [f] #(min (f) (f)))
(defn- adv [f] #(max (f) (f)))

(declare ^:private parse-roll-unit)

(defn dice->roll-fn [s]
  (comp :total (dice/->roll-fn s)))

(defn- parse-roll-vec [roll default-fn]
  (let [[f & args] (map #(parse-roll-unit % default-fn) roll)]
    (apply f args)))

(defn- parse-roll-unit [roll-unit default-fn]
  (cond
    (vector? roll-unit) (parse-roll-vec roll-unit default-fn)
    (nil? roll-unit) default-fn
    (keyword? roll-unit) (case roll-unit
                          :default (fn [] default-fn)
                          :disadvantaged disadv
                          :advantaged adv
                          :* (fn [multiplier factor]
                               (fn [] (* (multiplier) (factor))))
                          :constant (fn [x] (fn [] x)))
    (number? roll-unit) roll-unit))

(defn roll->fn [roll default-roll] ;TODO rename to amount?
  (parse-roll-vec (walk/prewalk #(if (string? %) (keyword %) %) (or roll [:default]))
                  (dice->roll-fn default-roll)))
