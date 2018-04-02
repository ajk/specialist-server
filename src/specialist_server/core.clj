(ns specialist-server.core
  (:require [clojure.walk :as walk]
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
        (-> op-map vals first (r/run context info))

        (not (nil? op))
        (r/run op context info)

        :else
        (throw (ex-info (str "Query error: no such operation") {:name op-name}))))

    (catch clojure.lang.ExceptionInfo ex
      (.write *out*
              (with-out-str
                (println (:id info) ">>>")
                (println (.getMessage ex))
                (pprint (ex-data ex))
                (println "<<<" (:id info))))
      (flush)
      {:errors [{:message (.getMessage ex)
                 :query_id (:id info)}]})))

(defn server
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
                  :root-value (or root {})}]
        (execute (or query preparsed) queryName (or context {}) info)))))

(defn executor
  "Alias for specialist-server.core/server"
  [schema]
  (server schema))
