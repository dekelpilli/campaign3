(ns campaign3.prompting
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import (de.codeshelf.consoleui.prompt ConsolePrompt CheckboxResult InputResult ListResult)
           (jline.console.completer StringsCompleter Completer)
           (java.util Collection)))

(def ^:private console-prompt (ConsolePrompt.))

(defrecord CommaSeparatedStringCompleter [lowers-set lowers-regular-map once?]
  Completer
  (complete [_ buffer cursor candidates]
    (let [listed (when-not (str/blank? buffer)
                   (cond-> (str/split buffer #",")
                           (str/ends-with? (str/trimr buffer) ",") (conj "")))
          current-listed-raw (when listed (nth listed (dec (count listed))))
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
  ([prompt valid-inputs & {:keys [completer]
                           :or   {completer :regular}}]
   (let [prompt-builder (.getPromptBuilder console-prompt)
         m (into {} (map (juxt str/lower-case identity)) valid-inputs)
         s (into (sorted-set) (keys m))]
     (-> prompt-builder
         (.createInputPrompt)
         (.message prompt)
         (.name prompt)
         (cond-> valid-inputs (.addCompleter
                                (case completer
                                  :regular (StringsCompleter. ^Collection valid-inputs) ;TODO make case insensitive
                                  :comma-separated (CommaSeparatedStringCompleter. s m false)
                                  :comma-separated-once (CommaSeparatedStringCompleter. s m true))))
         (.addPrompt))
     (when-let [input (-> (.prompt console-prompt (.build prompt-builder))
                          ^InputResult (get prompt)
                          (.getInput))]
       (case completer
         :regular (cond-> (str/trimr input)
                          valid-inputs (valid-inputs)) ;TODO valid-inputs (m) once case-insensitive
         (:comma-separated :comma-separated-once) (->> (str/split input #",")
                                                       (into #{} (comp (map (comp m str/lower-case str/trim))
                                                                       (filter identity)))))))))

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

(defn >>item
  ([coll] (>>item "Choose one from these:" coll))
  ([prompt coll & {:keys [sorted?]
                   :as   opts}]
   (let [prompt-builder (.getPromptBuilder console-prompt)
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
