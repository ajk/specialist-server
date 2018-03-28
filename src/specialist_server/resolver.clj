(ns specialist-server.resolver
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [specialist-server.type :as t]))


(defn resolve-field-var [scalar? fun-var & args]
  ;; Run conformers for both input and output.
  (let [croak #(throw (IllegalArgumentException. "missing :args and/or :ret in fspec"))
        fspec (or (spec/get-spec fun-var) (croak))
        c-args (spec/conform (or (:args fspec) (croak)) (vec args))
        type-name (-> fun-var meta :name str)
        valid-res (fn [res]
                    (cond
                      (not (spec/valid? (or (:ret fspec) (croak)) res))
                      (throw (ex-info (str "failed to validate return value for " fun-var)
                                      (select-keys (spec/explain-data (:ret fspec) res) [::spec/problems])))

                      (and scalar? (or (map? res) (and (coll? res) (-> res first map?))))
                      (throw (ex-info
                               (str "invalid query on " type-name ": "
                                    "the resolver returned a map or list but a scalar value was queried.")
                               (select-keys (last args) [:path])))

                      (map? res)
                      (assoc res :__typename type-name)

                      (and (coll? res) (-> res first map?))
                      (map #(assoc % :__typename type-name) res)

                      :else res))]

    (if (= c-args ::spec/invalid)
      (throw (ex-info (str "failed to conform arguments for " fun-var)
                      (select-keys (spec/explain-data (:args fspec) (vec args)) [::spec/problems])))
      (let [res (apply (deref fun-var) c-args)]
        (if (fn? res)
          ;; Allow for batch-loader to collect more nodes, return closure here and run later.
          (fn [] (valid-res (res)))
          (valid-res res))))))

(defn resolve-field [scalar? field-val-or-var root-val args context info]
  (if-not (var? field-val-or-var) ;regular value or resolver var
    field-val-or-var
    (resolve-field-var scalar?
                       field-val-or-var
                       root-val
                       args
                       context
                       info)))

(defn field-args [arg-map vars]
  (reduce-kv
    (fn [m k v]
      (assoc m k (if (keyword? v) (get vars v) v)))
    {} arg-map))

(defn run [[op query-args schema-sel-set] context {:keys [schema variable-values root-value] :as info}]
  (let [vars (reduce-kv
               (fn [m k v]
                 (assoc m k (get variable-values k (:default v))))
               {} query-args)

        queue-tuples (fn [key-root parent sel-set]
                       (map (fn [[[in-name out-name] args children]]
                              (if-let [node (get parent in-name)]
                                (list (conj key-root out-name)
                                      (resolve-field (empty? children)
                                                     node
                                                     parent
                                                     (field-args args vars)
                                                     context
                                                     (assoc info :field-name out-name :path key-root))
                                      children)
                                (throw (ex-info (str "Parse error: no such field " in-name " in " (pr-str key-root)) {}))))
                            sel-set))]

    (loop [out-queue [{}
                      (transient
                        (mapv (fn [[[in-name out-name] args children]]
                                (if-let [node (get-in schema [op in-name])]
                                  (list [out-name]
                                        (resolve-field (empty? children)
                                                       node
                                                       root-value
                                                       (field-args args vars)
                                                       context
                                                       (assoc info :field-name out-name :path []))
                                        children)
                                  (throw (ex-info (str "Parse error: no such field " in-name " in schema") {}))))
                              schema-sel-set))]]
      (let [out   (first out-queue)
            queue (persistent! (last out-queue))]
        (if (empty? queue)
          out
          (recur (reduce (fn [[coll q] [k node children]]
                           (let [v (if (fn? node) (node) node)]
                             (cond
                               (not (coll? v))
                               [(assoc-in coll k v)
                                q]

                               (map? v)
                               [(assoc-in coll k {})
                                (reduce conj! q (queue-tuples k v children))]

                               :else
                               [(assoc-in coll k [])
                                (reduce #(reduce conj! %1 %2)
                                        q
                                        (map (fn [i j] (queue-tuples (conj k i) j children))
                                             (-> v count range) v))])))
                         [out (transient [])]
                         queue)))))))
