(ns specialist-server.core
  (:require [clojure.walk :as walk]
            [specialist-server.parser :as p]
            [specialist-server.introspection :as i]))

;; If there are any unevaluated resolver functions, run them now
(defn run-fns [node]
  (if (fn? node) (node) node))

(defn- execute [query schema context info]
  (let [fun (p/parse query)]
    (try
      (if (:deferred? info)
        (walk/prewalk
          run-fns
          (walk/postwalk
            run-fns
            (fun schema context info)))
        (fun schema context info))
      (catch IllegalArgumentException ex
        (prn ex)
        {:errors [{:message (.getMessage ex)}]}))))

(defn executor [schema]
  (let [type-map (i/type-map schema)
        ; Internal schema map: add introspection fields.
        inner-schema (-> schema
                         (assoc-in [:query :__schema] #'i/__schema)
                         (assoc-in [:query :__type]   #'i/__type))]
    (fn [{:keys [query variables root context deferred?]}]
      (let [info {:id (str (java.util.UUID/randomUUID))
                  :schema schema
                  :type-map type-map
                  :variable-values (reduce-kv (fn [m k v] (assoc m (keyword k) v )) {} variables)
                  :root-value (or root {})
                  :deferred? (boolean deferred?)}]
        (execute query inner-schema (or context {}) info)))))
