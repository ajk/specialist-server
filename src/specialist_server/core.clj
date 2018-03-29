(ns specialist-server.core
  (:require [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [specialist-server.parser :as p]
            [specialist-server.resolver :as r]
            [specialist-server.introspection :as i]))

(defn- execute [query op-name context info]
  (try
    (if (nil? op-name)
      (-> query p/parse vals first (r/run context info))
      (if-let [op (-> query p/parse (get op-name))]
        (r/run op context info)
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
  [schema] ;TODO pre-parse queries
  (let [type-map (i/type-map schema)]
    (fn [{:keys [query variables root context queryName]}]
      (let [info {:id (str (java.util.UUID/randomUUID))
                  :schema (-> schema
                              (assoc-in [:query :__schema] #'i/__schema)
                              (assoc-in [:query :__type]   #'i/__type))
                  :type-map type-map
                  :variable-values (reduce-kv (fn [m k v] (assoc m (keyword k) v )) {} variables)
                  :root-value (or root {})}]
        (execute query queryName (or context {}) info)))))


(defn executor
  "Alias for specialist-server.core/server"
  [schema]
  (server schema))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#_(defn- deferred
  "Run deferred resolver functions. Uses breadth-first traversal."
  [data]
  (letfn [(tuples [key-root data]
            (if (map? data)
              (map (fn [[k v]] [(conj key-root k) v]) data)
              [[key-root data]]))]

    (loop [data-queue [{} (tuples [] data)]]
      (let [[out queue] data-queue]
        (if (empty? queue)
          out
          (recur (reduce (fn [[coll q] [k node]]
                           (let [v (if (fn? node) (node) node)]
                             (cond
                               (not (coll? v))
                               [(assoc-in coll k v)
                                q]

                               (map? v)
                               [(assoc-in coll k {})
                                (into q (tuples k v))]

                               :else
                               [(assoc-in coll k [])
                                (reduce into q (map #(tuples (conj k %1) %2) (range 0 (count v)) v))])))
                         [out []]
                         queue)))))))

#_(defn- execute [query schema context info]
  (try
    (let [fun (p/parse query)]
      (if (:deferred? info)
        (deferred (fun schema context info))
        (fun schema context info)))
    (catch clojure.lang.ExceptionInfo ex
      (.write *out*
              (with-out-str
                (println (:id info) ">>>")
                (println (.getMessage ex))
                (pprint (ex-data ex))
                (println "<<<" (:id info))
                ))
      (when *flush-on-newline* (flush))
      {:errors [{:message (.getMessage ex)
                 :query_id (:id info)}]})))

#_(defn server [schema]
  (let [type-map (i/type-map schema)
        ; Internal schema map: add introspection fields.
        inner-schema (-> schema
                         (assoc-in [:query :__schema] #'i/__schema)
                         (assoc-in [:query :__type]   #'i/__type))]
    (fn [{:keys [query variables root context deferred?]}]
      (let [info {:id (str (java.util.UUID/randomUUID))
                  :schema schema
                  :type-map type-map
                  :variable-values (or variables {}) ;(reduce-kv (fn [m k v] (assoc m (keyword k) v )) {} variables)
                  :root-value (or root {})
                  :deferred? (boolean deferred?)}]
        (execute query inner-schema (or context {}) info)))))
