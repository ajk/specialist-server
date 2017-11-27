(ns specialist-server.core
  (:require [clojure.walk :as walk]
            [specialist-server.parser :as p]
            [specialist-server.introspection :as i]))


(defn- deferred
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

(defn- execute [query schema context info]
  (let [fun (p/parse query)]
    (try
      (if (:deferred? info)
        (deferred (fun schema context info))
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
