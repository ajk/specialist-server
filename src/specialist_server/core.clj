(ns specialist-server.core
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [specialist-server.parser :as p]
            [specialist-server.resolver :as r]
            [specialist-server.introspection :as i]))


(defn- execute [query op-name context info]
  (try
    (let [op-map (if (map? query) query (p/parse query))
          op (get op-map op-name)]
      (cond
        (and (nil? op-name) (string? query))
        (do
          (log/debug "Parse and run query:" query)
          (-> op-map vals first (r/run context info)))

        (not (nil? op))
        (do
          (log/debug "Run query:" op-name)
          (r/run op context info))

        :else
        (throw (ex-info (str "Query error: no such operation") {:name op-name}))))

    (catch clojure.lang.ExceptionInfo ex
      (log/error
        (with-out-str
          (println (:id info) ">>>")
          (println (.getMessage ex))
          (pprint (ex-data ex))
          (println "<<<" (:id info))))
      {:errors [{:message (.getMessage ex)
                 :query_id (:id info)}]})))

(defn server
  "Creates GraphQL server fn from schema.

   Usage:
   (def graphql (server {:query {...} :mutation {...}}))
   (graphql {:query \"query FooQuery {...}\" :context my-ctx :root {}})

   You can also pass in query strings which are pre-parsed and can be called by name (similiar to stored procedures):

   (def graphql-pre (server {:query {...} :mutation {...}} (slurp path-to-queries-1) ... (slurp path-to-queries-N)))
   (graphql-pre {:queryName \"FooQuery\" :variables {...} :context my-ctx :root {}})
  "
  [schema & pre-strings]
  (let [preparsed (reduce (fn [coll s] (merge coll (p/parse s))) {} pre-strings)
        type-map (i/type-map schema)]
    (fn [{:keys [query variables root context queryName]}]
      (let [info {:id (str (java.util.UUID/randomUUID))
                  :schema (-> schema
                              (assoc-in [:query :__schema] #'i/__schema)
                              (assoc-in [:query :__type]   #'i/__type))
                  :type-map type-map
                  :variable-values (reduce-kv (fn [m k v] (assoc m (keyword k) v )) {} variables)
                  :validate-output? (get context :validate-output? true)
                  :root-value (or root {})}]
        (execute (or query preparsed) queryName (or context {}) info)))))

(defn executor
  "Alias for specialist-server.core/server"
  [schema]
  (server schema))
