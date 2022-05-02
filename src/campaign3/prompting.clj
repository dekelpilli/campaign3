(ns campaign3.prompting
  (:require [clojure.string :as str])
  (:import (de.codeshelf.consoleui.prompt CheckboxResult ConsolePrompt InputResult ListResult)
           (jline.console.completer Completer)))

(def ^:private console-prompt (ConsolePrompt.))
(def ^:private input-threshold 10)
(def default-opts {:completer :regular
                   :sorted?   true})

(defrecord CommaSeparatedStringsCompleter [lowers-set lowers-regular-map once?]
  Completer
  (complete [_ buffer cursor candidates]
    (let [listed (when-not (str/blank? buffer)
                   (cond-> (str/split buffer #",")
                           (str/ends-with? (str/trimr buffer) ",") (conj "")))
          current-listed-raw (peek listed)
          current-listed-lower (when current-listed-raw (-> current-listed-raw str/trim str/lower-case))
          current-listed-complete? (lowers-set current-listed-lower)
          current-listed (if current-listed-complete? nil current-listed-lower)
          options (if (and once? listed)
                    (apply disj lowers-set (map (comp str/lower-case str/trim) listed))
                    lowers-set)]
      (if (str/blank? current-listed)
        (.addAll candidates (map lowers-regular-map options))
        (let [matching-uppers (for [potential-candidate-lower (subseq options >= current-listed)
                                    :while (str/starts-with? potential-candidate-lower current-listed)]
                                (get lowers-regular-map potential-candidate-lower))]
          (.addAll candidates matching-uppers)))
      (if current-listed-complete?
        cursor
        (cond-> (- cursor (count current-listed-raw))
                (> (count listed) 1) inc)))))

(defrecord CaseInsensitiveStringsCompleter [lowers-set lowers-regular-map]
  Completer
  (complete [_ buffer _cursor candidates]
    (if buffer
      (let [buffer-lower (str/lower-case buffer)
            matching-uppers (for [potential-candidate-lower (subseq lowers-set >= buffer-lower)
                                  :while (str/starts-with? potential-candidate-lower buffer-lower)]
                              (get lowers-regular-map potential-candidate-lower))]
        (.addAll candidates matching-uppers))
      (.addAll candidates (vals lowers-regular-map)))
    (if (empty? candidates) -1 0)))

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

(defn >>input
  ([] (>>input "Enter text:"))
  ([prompt] (>>input prompt nil))
  ([prompt coll & {:as opts}]
   (let [console-prompt (ConsolePrompt.)
         {:keys [completer]} (merge default-opts opts)
         prompt-builder (.getPromptBuilder console-prompt)
         stringified-map (->stringified-map coll opts)
         valid-inputs (set (keys stringified-map))
         m (into {} (map (juxt str/lower-case identity)) valid-inputs)
         value-mapper (cond->> identity
                               coll (comp m)
                               (map? coll) (comp coll)
                               coll (comp stringified-map))
         s (into (sorted-set) (keys m))]
     (-> prompt-builder
         (.createInputPrompt)
         (.message prompt)
         (.name prompt)
         (cond-> valid-inputs (.addCompleter
                                (case completer
                                  :regular (CaseInsensitiveStringsCompleter. s m)
                                  :comma-separated (CommaSeparatedStringsCompleter. s m false)
                                  :comma-separated-once (CommaSeparatedStringsCompleter. s m true))))
         (.addPrompt))
     (when-let [input (-> (.prompt console-prompt (.build prompt-builder))
                          ^InputResult (get prompt)
                          (.getInput))]
       (case completer
         :regular (value-mapper (str/lower-case (str/trimr input)))
         (:comma-separated :comma-separated-once) (->> (str/split input #",")
                                                       (keep (comp value-mapper str/lower-case str/trim))))))))

(defn >>distinct-items
  ([coll] (>>distinct-items "Choose all that apply:" coll))
  ([prompt coll & {:as opts}]
   (let [opts (merge default-opts opts)
         m (->stringified-map coll opts)]
     (if (> (count m) input-threshold)
       (->> (>>input prompt m :completer :comma-separated-once)
            (set))
       (let [prompt-builder (.getPromptBuilder console-prompt)]
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
             (->> (into #{} (map m)))))))))

(defn >>item
  ([coll] (>>item "Choose one from these:" coll))
  ([prompt coll & {:as opts}]
   (let [{:keys [sorted?]} (merge default-opts opts)
         m (if (map? coll)
             (stringify-keys opts coll)
             (into (if sorted? (sorted-map) {}) (map (juxt stringify identity)) coll))]
     (if (> (count m) input-threshold)
       (>>input prompt m)
       (let [prompt-builder (.getPromptBuilder console-prompt)]
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
