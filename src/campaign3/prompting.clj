(ns campaign3.prompting
  (:import (de.codeshelf.consoleui.prompt ConsolePrompt CheckboxResult InputResult)))

(def ^:private console-prompt (ConsolePrompt.))

(defn >>checkbox
  ([coll] (>>checkbox "Choose all that apply: " coll))
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
                         coll))
         (.addPrompt))
     (-> (.prompt console-prompt (.build prompt-builder))
         ^CheckboxResult (get prompt)
         (.getSelectedIds)
         (set)))))

(defn >>number
  ([] (>>number "Enter number: "))
  ([prompt]
   (let [prompt-builder (.getPromptBuilder console-prompt)]
     (-> prompt-builder
         (.createInputPrompt)
         (.message prompt)
         (.name prompt)
         (.addPrompt))
     (-> (.prompt console-prompt (.build prompt-builder))
         ^InputResult (get prompt)
         (.getInput)
         (parse-long)))))
