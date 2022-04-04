(ns campaign3.db
  (:require [honey.sql :as hsql]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :refer [as-unqualified-kebab-maps]]
            [config.core :refer [env]])
  (:import (java.sql Connection)))

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
     (binding [*txn* txn])
     ~@body))

(defn execute!
  ([statement] (execute! statement nil))
  ([statement return-keys]
   (jdbc/execute!
     (or *txn* c)
     (hsql/format statement)
     (cond-> {:builder-fn as-unqualified-kebab-maps}
             (seq return-keys) (assoc :return-keys (mapv (comp first hsql/format-expr) return-keys))))))
