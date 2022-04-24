(ns campaign3.prompting
  (:require [clojure.string :as str])
  (:import (de.codeshelf.consoleui.prompt ConsolePrompt CheckboxResult InputResult ListResult)
           (jline.console.completer Completer)))

(def ^:private console-prompt (ConsolePrompt.))
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
          options (when listed (apply disj lowers-set (map (comp str/lower-case str/trim) listed)))]
      (if current-listed
        (let [matching-uppers (for [potential-candidate-lower (subseq options >= current-listed)
                                    :while (str/starts-with? potential-candidate-lower current-listed)]
                                (get lowers-regular-map potential-candidate-lower))]
          (.addAll candidates matching-uppers))
        (.addAll candidates (map lowers-regular-map options)))
      (if current-listed-complete?
        cursor
        (cond-> (- cursor (count current-listed-raw))
                (and (some? listed) (> (count listed) 1)) inc)))))

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
  ([] (>>input "Enter text: "))
  ([prompt] (>>input prompt nil))
  ([prompt valid-inputs & {:as opts}]
   (let [{:keys [completer]} (merge default-opts opts)
         prompt-builder (.getPromptBuilder console-prompt)
         valid-inputs (set valid-inputs)
         m (into {} (map (juxt str/lower-case identity)) valid-inputs)
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
         :regular (m (str/lower-case (str/trimr input)))
         (:comma-separated :comma-separated-once) (->> (str/split input #",")
                                                       (into #{} (comp (map (comp m str/lower-case str/trim))
                                                                       (filter identity)))))))))

(defn >>checkbox
  ([coll] (>>checkbox "Choose all that apply:" coll))
  ([prompt coll & {:as opts}]
   (let [opts (merge default-opts opts)
         prompt-builder (.getPromptBuilder console-prompt)
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
         (->> (into #{} (map m)))))))

(defn >>item
  ([coll] (>>item "Choose one from these:" coll))
  ([prompt coll & {:as opts}]
   (let [{:keys [sorted?]} (merge default-opts opts)
         prompt-builder (.getPromptBuilder console-prompt)
         m (if (map? coll)
             (stringify-keys opts coll)
             (into (if sorted? (sorted-map) {}) (map (juxt stringify identity)) coll))]
     (if (> (count m) 10)
       (->> (keys m)
            (>>input prompt)
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
