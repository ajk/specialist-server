(ns specialist-server.resolver
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as spec]
            [specialist-server.type :as t]))


(defn valid-res [fun-var ret-spec res info]
  (let [m (meta fun-var)
        type-name (str (get m ::t/name (:name m)))]
    (if (or (not (:validate-output? info)) (spec/valid? ret-spec res))
      (if (coll? res)
        (vary-meta res assoc ::type-name type-name)
        res)
      (throw (ex-info (str "failed to validate return value for " fun-var " via " (-> info :path pr-str))
                      (select-keys (spec/explain-data ret-spec res) [::spec/problems]))))))

(defn resolve-field [field & args]
  (if-not (var? field) ;regular value or resolver var
    field
    (let [fspec     (or (spec/get-spec field)
                        (throw (IllegalArgumentException. (str "missing fspec for " field))))
          ret-spec  (or (:ret fspec)
                        (throw (IllegalArgumentException. (str "missing :ret in fspec for " field))))
          args-spec (or (:args fspec)
                        (throw (IllegalArgumentException. (str "missing :args in fspec for " field))))
          c-args (spec/conform args-spec (vec args))
          info (last c-args)]

      (if (= c-args ::spec/invalid)
        (throw (ex-info (str "failed to conform arguments for " field)
                        (select-keys (spec/explain-data args-spec (vec args)) [::spec/problems])))
        (let [res (try
                    (apply (deref field) c-args)
                    (catch Exception e
                      (log/error "Exception in" field  "via" (-> info :path pr-str))
                      (log/error (.toString e))
                      (throw e)))]
          (if (fn? res)
            ;; Allow for batch-loader to collect more nodes, return closure here and run later.
            (fn []
              (valid-res field ret-spec (try
                                          (res)
                                          (catch Exception e
                                            (log/error "Exception in" field  "via" (-> info :path pr-str))
                                            (log/error (.toString e))
                                            (throw e)))
                         info))
            (valid-res field ret-spec res info)))))))

(defn field-args [arg-map vars]
  (reduce-kv
    (fn [m k v]
      (assoc m k (if (keyword? v) (get vars v) v)))
    {} arg-map))

(defn selections->queue->out [vars context info queue]
  (let [queue-tuples (fn [key-root parent sel-set]
                       (map (fn [[[in-name out-name] args children]]
                              (if (contains? parent in-name)
                                (list (conj key-root out-name)
                                      (resolve-field (get parent in-name)
                                                     parent
                                                     (field-args args vars)
                                                     context
                                                     (assoc info :field-name out-name :path key-root))
                                      children)
                                (throw (ex-info (str "Parse error: no such field " in-name " in " (pr-str key-root)) {}))))
                            sel-set))]
    (loop [out {:data {}}
           idx 0
           t-queue (transient queue)]

      (if-let [[k node children] (get t-queue idx)]
        (let [type-name (-> node meta ::type-name)
              v (if (fn? node) (node) node)]

          (when (and (empty? children) (or (map? v) (and (coll? v) (-> v first map?))))
            (throw (ex-info
                     (str "invalid query on " type-name ": "
                          "the resolver returned a map but fields are missing from query.")
                     {:path k})))
          (cond
            (or (not (coll? v)) (empty? children))
            (recur
              (assoc-in out k v)
              (inc idx)
              t-queue)

            (map? v)
            (recur
              (assoc-in out k {})
              (inc idx)
              (reduce conj! t-queue (queue-tuples k (assoc v :__typename type-name) children)))

            :else
            (recur
              (assoc-in out k [])
              (inc idx)
              (reduce #(reduce conj! %1 %2)
                      t-queue
                      (map (fn [i j] (queue-tuples (conj k i) (assoc j :__typename type-name) children))
                           (-> v count range) v)))))

        out))))

(defn run [[op query-args schema-sel-set] context {:keys [schema variable-values root-value] :as info}]
  (let [vars (reduce-kv
               (fn [m k v]
                 (assoc m k (get variable-values k (:default v))))
               {} query-args)]

    (selections->queue->out
      vars context info
      (mapv (fn [[[in-name out-name] args children]]
              (if-let [node (get-in schema [op in-name])]
                (list [:data out-name]
                      (resolve-field node
                                     root-value
                                     (field-args args vars)
                                     context
                                     (assoc info :field-name out-name :path []))
                      children)
                (throw (ex-info (str "Parse error: no such field " in-name " in schema") {}))))
            schema-sel-set))))
