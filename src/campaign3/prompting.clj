(ns campaign3.prompting
  (:import (de.codeshelf.consoleui.prompt ConsolePrompt)))

;TODO use old prompting if called from repl

(def ^:private prompt (ConsolePrompt.))

(defn choose-multi [coll]
  (let [prompt-builder (.getPromptBuilder prompt)]
    (-> prompt-builder
        (.createCheckboxPrompt)
        (.message "Choose all that apply: ")
        (as-> builder (reduce
                        #(doto %1 (-> (.newItem %2)
                                      (.text %2)
                                      (.add)))
                        builder
                        coll))
        (.addPrompt))
    (doto (.prompt prompt (.build prompt-builder))
      println)))
