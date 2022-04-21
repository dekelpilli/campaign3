(ns campaign3.db
  (:require [honey.sql :as hsql]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :refer [ReadableColumn as-unqualified-kebab-maps]]
            [next.jdbc.prepare :refer [SettableParameter]]
            [config.core :refer [env]]
            [jsonista.core :as j])
  (:import (java.sql Connection PreparedStatement)
           (org.postgresql.util PGobject PSQLException)
           (clojure.lang IObj IPersistentMap IPersistentVector)))

(def data-src (let [{:keys [db-host db-port db-user db-pass db-name]} env]
                (jdbc/get-datasource {:host     db-host
                                      :port     db-port
                                      :user     db-user
                                      :password db-pass
                                      :dbtype   "postgres"
                                      :dbname   db-name})))

(defn connect ^Connection [] (jdbc/get-connection data-src))

(def ^:private c (connect))
(def ^:dynamic *txn* nil)

(defmacro in-transaction [& body]
  `(jdbc/with-transaction
     [~'txn data-src]
     (binding [*txn* ~'txn]
       ~@body)))

(defn execute!
  ([statement] (execute! statement nil))
  ([statement return-keys]
   (jdbc/execute!
     (or *txn* c)
     (hsql/format statement)
     (cond-> {:builder-fn as-unqualified-kebab-maps}
             (seq return-keys) (assoc :return-keys (mapv (comp first hsql/format-expr) return-keys))))))

(defn load-all [table]
  (try
    (execute! {:select [:*] :from [table]})
    (catch PSQLException _ [])))

(defn- ->pgobject  [x]
  (let [pg-type (or (:pg-type (meta x)) "jsonb")]
    (doto (PGobject.)
      (.setType pg-type)
      (.setValue (j/write-value-as-string x)))))

(defn- <-pgobject
  [^PGobject v]
  (let [type (.getType v)
        value (.getValue v)]
    (if (#{"jsonb" "json"} type)
      (let [v (j/read-value value j/keyword-keys-object-mapper)]
        (cond-> v
                (instance? IObj v) (with-meta {:pg-type type})))
      value)))

(extend-protocol SettableParameter
  IPersistentMap
  (set-parameter [m ^PreparedStatement s i]
    (.setObject s i (->pgobject m)))

  IPersistentVector
  (set-parameter [v ^PreparedStatement s i]
    (.setObject s i (->pgobject v))))

(extend-protocol ReadableColumn
  PGobject
  (read-column-by-label [^PGobject v _] (<-pgobject v))
  (read-column-by-index [^PGobject v _ _] (<-pgobject v)))
