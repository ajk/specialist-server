(ns specialist-server.core
  (:require [specialist-server.parser :as p]
            [specialist-server.introspection :as i]))

(defn- execute [query schema context info]
  (try
    ((p/parse query) schema context info)
    (catch IllegalArgumentException ex
      {:errors [{:message (.getMessage ex)}]})))

(defn executor [schema]
  (let [type-map (i/type-map schema)
        ; Internal schema map: add introspection fields.
        inner-schema (-> schema
                         (assoc-in [:query :__schema] #'i/__schema)
                         (assoc-in [:query :__type]   #'i/__type))]
    (fn [{:keys [query variables root context]}]
      (let [info {:schema schema
                  :type-map type-map
                  :variable-values (reduce-kv (fn [m k v] (assoc m (keyword k) v )) {} variables)
                  :root-value (or root {})}]
        (execute query inner-schema (or context {}) info)))))