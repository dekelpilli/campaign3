(ns campaign3.io
  (:require #_[enquirer :as e]
            [promesa.core :as p]))

;(defn- read-input [runnable]
;  (-> (.run runnable)
;      (.then identity)
;      (.catch js/console.error))) ;TODO promesa to block for value
;
;(defn input [p]
;  (-> (e/Input. {"type"    "input"
;                 "name"    "name"
;                 "message" p})
;      (read-input)))
;
;(defn select [prompt m]
;  (let [select (e/Select. {"name"    "select"
;                           "message" prompt
;                           "choices" (vec (keys m))})]
;    (get m (read-input select))))
;
;(defn select-optional [prompt m]
;  (select prompt (assoc m "_none" nil)))

(def x 123)
