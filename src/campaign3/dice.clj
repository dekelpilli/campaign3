(ns campaign3.dice
  (:require [org.fversnel.dnddice.core :as d]))

(defn roll [r]
  (update (d/roll r) :die-rolls seq))

(defn ->roll-fn [roll-string]
  (when roll-string
    (let [parsed (d/parse roll-string)]
      (if-not (string? parsed)
        #(d/roll parsed)
        (throw (ex-info "Failed to parse roll" {:roll-string roll-string
                                                :error       parsed}))))))
