(ns campaign3.prompting
  (:require [clojure.string :as str])
  (:import (de.codeshelf.consoleui.prompt ConsolePrompt CheckboxResult InputResult ListResult)
           (jline.console.completer StringsCompleter)
           (java.util Collection)))

(def ^:private console-prompt (ConsolePrompt.))

;TODO custom completer for comma-separated input version of checkbox

(defn- stringify [x]
  (if (keyword? x) (name x) (str x)))

(defn- stringify-keys [{:keys [sorted?]} m]
  (into (if sorted? (sorted-map) {})
        (map (juxt (comp stringify key) val))
        m))

(defn- ->stringified-map [coll opts]
  (if (map? coll)
    (stringify-keys opts coll)
    (into (if sorted? (sorted-map) {}) (map (juxt stringify identity)) coll)))

(defn >>checkbox
  ([coll] (>>checkbox "Choose all that apply:" coll))
  ([prompt coll & {:keys [sorted?]
                   :as   opts}]
   (let [prompt-builder (.getPromptBuilder console-prompt)
         m (->stringified-map coll opts)]
     (-> prompt-builder
         (.createCheckboxPrompt)
         (.message prompt)
         (as-> builder (reduce
                         #(doto %1 (-> (.newItem %2)
                                       (.text %2)
                                       (.add)))
                         builder
                         (keys m)))
         (.addPrompt))
     (-> (.prompt console-prompt (.build prompt-builder))
         ^CheckboxResult (get prompt)
         (.getSelectedIds)
         (->> (into (if sorted? (sorted-set) #{}) (map m)))))))

(defn >>input
  ([] (>>input "Enter text: "))
  ([prompt] (>>input prompt nil))
  ([prompt valid-inputs]
   (let [prompt-builder (.getPromptBuilder console-prompt)]
     (-> prompt-builder
         (.createInputPrompt)
         (.message prompt)
         (.name prompt)
         (cond-> valid-inputs (.addCompleter (StringsCompleter. ^Collection valid-inputs)))
         (.addPrompt))
     (-> (.prompt console-prompt (.build prompt-builder))
         ^InputResult (get prompt)
         (.getInput)))))

(defn >>item
  ([coll] (>>item "Choose one from these:" coll))
  ([prompt coll & {:keys [sorted?]
                   :as   opts}]
   (let [prompt-builder (.getPromptBuilder console-prompt)
         m (if (map? coll)
             (stringify-keys opts coll)
             (into (if sorted? (sorted-map) {}) (map (juxt stringify identity)) coll))]
     (if (> (count m) 10)
       (some->> (keys m)
                (>>input prompt)
                (str/trimr)
                (get m))
       (do
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
             (m)))))))
