(ns campaign3.prompting
  (:require [clojure.walk :as walk])
  (:import (de.codeshelf.consoleui.prompt ConsolePrompt CheckboxResult InputResult ListResult)))

(def ^:private console-prompt (ConsolePrompt.))

(defn- stringify [x]
  (if (keyword? x) (name x) (str x)))

(defn- stringify-keys [{:keys [sorted?]} m]
  (walk/postwalk (fn [x] (if (map? x)
                           (into (if sorted? (sorted-map) {})
                                 (map (juxt (comp stringify key) val))
                                 x)
                           x))
                 m))

(defn >>checkbox
  ([coll] (>>checkbox "Choose all that apply:" coll))
  ([prompt coll]
   (let [prompt-builder (.getPromptBuilder console-prompt)]
     (-> prompt-builder
         (.createCheckboxPrompt)
         (.message prompt)
         (as-> builder (reduce
                         #(doto %1 (-> (.newItem %2)
                                       (.text %2)
                                       (.add)))
                         builder
                         (map stringify coll)))
         (.addPrompt))
     (-> (.prompt console-prompt (.build prompt-builder))
         ^CheckboxResult (get prompt)
         (.getSelectedIds)
         (set)))))

(defn >>item
  ([coll] (>>item "Choose one from these:" coll))
  ([prompt coll & {:keys [sorted?]
                   :as   opts}]
   ;TODO when (count coll) is above a certain number (10?), use input + autocomplete instead of list
   (let [prompt-builder (.getPromptBuilder console-prompt)
         m (if (map? coll)
             (stringify-keys opts coll)
             (into (if sorted? (sorted-map) {}) (map (juxt stringify identity)) coll))]
     (-> prompt-builder
         (.createListPrompt)
         (.message prompt)
         (as-> builder (reduce
                         #(doto %1 (-> (.newItem %2)
                                       (.text %2)
                                       (.add)))
                         builder
                         (-> m (keys) (conj "\u001B[31mNone\u001B[0m"))))
         (.addPrompt))
     (-> (.prompt console-prompt (.build prompt-builder))
         ^ListResult (get prompt)
         (.getSelectedId)
         (m)))))

(defn >>input
  ([] (>>input "Enter text: "))
  ([prompt]
   (let [prompt-builder (.getPromptBuilder console-prompt)]
     (-> prompt-builder
         (.createInputPrompt)
         (.message prompt)
         (.name prompt)
         (.addPrompt))
     (-> (.prompt console-prompt (.build prompt-builder))
         ^InputResult (get prompt)
         (.getInput)))))
