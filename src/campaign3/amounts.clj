(ns campaign3.amounts
  (:require [clojure.walk :as walk]
            [org.fversnel.dnddice.core :as d]))

(defn- disadv [f] #(min (f) (f)))
(defn- adv [f] #(max (f) (f)))

(declare ^:private parse-amount-unit)

(defn- ->roll-fn [roll-string]
  (when roll-string
    (let [parsed (d/parse roll-string)]
      (if-not (string? parsed)
        #(d/roll parsed)
        (throw (ex-info "Failed to parse roll" {:roll-string roll-string
                                                :error       parsed}))))))

(defn- dice->fn [s]
  (comp :total (->roll-fn s)))

(defn- parse-amount-vec [amount default-fn]
  (let [[f & args] (map #(parse-amount-unit % default-fn) amount)]
    (apply f args)))

(defn- parse-amount-unit [amount-unit default-fn]
  (cond
    (vector? amount-unit) (parse-amount-vec amount-unit default-fn)
    (nil? amount-unit) default-fn
    (keyword? amount-unit) (case amount-unit
                             :default (fn [] default-fn)
                             :disadvantaged disadv
                             :advantaged adv
                             :* (fn [multiplier factor]
                                  (fn [] (* (multiplier) (factor))))
                             :constant (fn [x] (fn [] x)))
    (number? amount-unit) amount-unit))

(defn amount->fn [amount default-roll]
  (parse-amount-vec (walk/prewalk #(if (string? %) (keyword %) %) (or amount [:default]))
                    (dice->fn default-roll)))


