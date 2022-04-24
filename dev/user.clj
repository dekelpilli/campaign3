(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.string :as str]
            [randy.core :as r]
            [campaign3
             [db-data :as dbd]
             [util :as u]
             [prompting :as p]
             [db :as db]
             [randoms :as randoms]
             [curios :as curios]
             [relics :as relics]
             [mundanes :as mundanes]
             [enchants :as enchants]
             [crafting :as crafting]
             [consumables :as consumables]
             [paths :as paths]
             [encounters :as encounters]
             [dice :as dice]
             [rings :as rings]
             [uniques :as uniques]
             [core :as core]]))

;https://asciinema.org/a/296507
(def safe-requires
  "List of namespaces to require and refer when inside user ns at load time.
   Can be given an initialization body to execute after having been required.
   To do so, wrap the lib spec in a vector, and all elements after the lib
   spec vector will be evaled after the lib spec has been required."
  '[[clojure.repl :as repl :refer (source apropos dir pst doc find-doc)]
    [clojure.java.javadoc :as javadoc :refer (javadoc)]
    [clojure.pprint :as pprint :refer (pp pprint)]
    [clojure.stacktrace :as stacktrace :refer (e)]
    [clojure.set :as set]
    [[io.aviso.repl :as aviso]
     ((resolve 'aviso/install-pretty-exceptions))]
    [[puget.printer :as puget :refer (cprint)]
     (add-tap (bound-fn* (resolve 'puget/cprint)))]])

(defn safe-require-init [req]
  (let [init? (-> req first vector?)
        lib (if init? (first req) req)
        init (when init? (rest req))]
    `(try
       (require '~lib)
       ~@init
       (catch Throwable t#
         (println ~(str "Error loading " lib ":")
                  (or (.getMessage t#)
                      (type t#)))))))
(defmacro safe-require-inits []
  `(do
     ~@(for [req safe-requires]
         (safe-require-init req))))
(safe-require-inits)

(defmacro cp [] `(cprint *1))

(def default-aliases (filter (comp #(str/starts-with? % "campaign3") str val)
                             (ns-aliases *ns*)))

(defmacro reload []
  (cons 'do
        (reduce (fn [l [alias-sym ns-sym]]
                  (->> l
                       (cons (list `require [`'~ns-sym :as `'~alias-sym]))
                       #_(cons (list `alias `'~alias-sym `'~ns-sym))
                       (cons (list `ns-unalias `*ns* `'~alias-sym))))
                '()
                (update-vals default-aliases (comp symbol str)))))

(defn rr []
  (refresh)
  (reload))
