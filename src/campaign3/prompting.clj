(ns campaign3.prompting
  (:require [clojure.walk :as walk])
  (:import (de.codeshelf.consoleui.prompt ConsolePrompt CheckboxResult InputResult ListResult)))

(def ^:private console-prompt (ConsolePrompt.))

(defn- stringify [x]
  (if (keyword? x) (name x) (str x)))

(defn- stringify-keys [m]
  (walk/postwalk (fn [x] (if (map? x) (into {} (map (juxt (comp stringify key) val) x)) x)) m))

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
  ([prompt coll & {:keys [sorted?]}]
   (let [prompt-builder (.getPromptBuilder console-prompt)
         m (if (map? coll)
             (stringify-keys coll)
             (into (if sorted? (sorted-map) {}) (map (juxt stringify identity)) coll))]
     (-> prompt-builder
         (.createListPrompt) ;TODO choice vs list?
         (.message prompt)
         (as-> builder (reduce
                         #(doto %1 (-> (.newItem %2)
                                       (.text %2)
                                       (.add)))
                         builder
                         (keys m)))
         (.addPrompt))
     (-> (.prompt console-prompt (.build prompt-builder))
         ^ListResult (get prompt)
         (.getSelectedId)
         (m)))))

(defn >>input
  ([] (>>input "Enter number: "))
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
