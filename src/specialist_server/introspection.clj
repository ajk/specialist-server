(ns specialist-server.introspection
  (:refer-clojure :exclude [type])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [specialist-server.type :as t]))

;TODO
; - Enum
; - Input object
; - Directive
; - Union (?)
; - Interface (?)

(def base-type
  {:kind t/scalar-kind
   :name nil
   :description nil
   :fields nil
   :interfaces nil
   :possibleTypes nil
   :enumValues nil
   :inputFields nil
   :ofType nil})

(def spec-ns "clojure.spec.alpha") ;TODO rename when not alpha

; set of specs we consider to be lists
(def coll-type? (->> '(* + coll-of every) (map #(symbol spec-ns (str %))) set))

;;;

(defn even->map [coll]
  (if-not (-> coll count even?)
    (throw (IllegalArgumentException. "even->map: input list must have even count")))
  (into {} (map vec (partition 2 coll))))

(defn get-spec [v]
  (s/form (s/get-spec v)))

(defn non-null [t]
  (-> base-type
      (assoc :kind t/non-null-kind)
      (assoc :ofType t)))

(defn field-meta [v]
  (if (seq v)
    (if (= 'specialist-server.type/field (first v))
      (-> v eval meta)
      (field-meta (second v)))))


(declare type)
(declare field)

;TODO Do we need to support more than spec/keys and :req-un + :opt-un?
(defmulti ret-keys (fn [v]
                     (cond
                       (var? v)     :var
                       (keyword? v) :kw
                       (seq? v)     :keys
                       :else :default)))

(defmethod ret-keys :var [v]
  (ret-keys (-> v get-spec rest even->map :ret)))

(defmethod ret-keys :kw [v]
  (ret-keys (get-spec v)))

(defmethod ret-keys :keys [v]
  (when (seq v)
    (if (= 'clojure.spec.alpha/keys (first v))
      (apply concat (-> v rest even->map (select-keys [:req-un :opt-un]) vals))
      (ret-keys (second v)))))

(defmethod ret-keys :default [v] nil)

;;;

;TODO Do we need to support more than spec/tuple?
;     Also, make opt-un keys nilable.
(defn arg-keys [v]
  (if-not (var? v) (throw (Exception. "arg-keys: input must be a var")))
  (let [a (-> v s/get-spec s/form rest even->map :args)]
    (if (= 'clojure.spec.alpha/tuple (first a))
      (->> a rest second ret-keys))))

(defn args [v]
  (->> v arg-keys
       (map field)
       (map #(select-keys % [:name :description :type]))
       (map #(assoc % :defaultValue nil)))) ;TODO support default values

;FIXME enums next
(defmulti type (fn [v]
                 (cond
                   (var? v)     :var
                   (keyword? v) :kw
                   (map? v)     :map
                   (seq? v)     :list
                   (symbol? v)  :sym
                   )))

(defmethod type :var [v]
  (let [v-meta (meta v)
        v-name (-> v-meta :name str)
        doc (:doc v-meta)
        ret (-> v get-spec rest even->map :ret)
        field-keys (ret-keys ret)]
    (if field-keys
      (-> base-type
          (assoc :kind t/object-kind)
          (assoc :name v-name)
          (assoc :description doc)
          (assoc :fields field-keys) ;just the keys, break infinite type -> fields -> type loops
          (assoc :interfaces [])
          non-null)
      (type ret))))

(defmethod type :kw [v]
  (let [spec (s/get-spec v)
        spec-meta (meta spec)]
    (if (or (::t/var spec-meta) (::t/kind spec-meta))
      (type spec-meta)
      (-> spec s/form type))))

(defmethod type :sym [v]
  (-> v resolve meta type))

(defmethod type :list [v]
  ;;TODO test more variations
  (cond
    (coll-type? (first v))
    (-> base-type
        (assoc :kind t/list-kind)
        (assoc :ofType (type (second v)))
        non-null)

    (= 'clojure.spec.alpha/nilable (first v))
    (:ofType (type (second v))) ;strip non-null type wrapper

    (= 'specialist-server.type/field (first v))
    (let [m (field-meta v)]
      (-> base-type
          (assoc :kind (::t/kind m))
          (assoc :name (::t/name m))
          (assoc :description (::t/type-description m))
          non-null))))


(defmethod type :map [v]
  (if (::t/var v)
    (type (::t/var v))
    (-> base-type
        (assoc :kind (::t/kind v))
        (assoc :name (::t/name v))
        (assoc :description (get v ::t/type-description (:doc v)))
        non-null)))

(defmethod type :default [v]
  (prn v)
  (throw (Exception. "type: don't know how to proceed")))

;;;

(defmulti field (fn [v]
                  (cond
                    (var? v)     :var
                    (keyword? v) :kw)))

(defmethod field :var [v]
  (let [v-meta (meta v)
        v-name (-> v-meta :name str)
        doc (:doc v-meta)
        depr (:deprecated v-meta)]
    {:name v-name
     :description doc
     :args (args v)
     :type (type v)
     :isDeprecated (boolean depr)
     :deprecationReason depr}))

(defmethod field :kw [v]
  (let [spec (s/get-spec v)
        m (or (meta spec) (some-> spec s/form field-meta))]
    (when (nil? m)
      (prn v)
      (prn "---"))
    (if (::t/var m)
      (-> (::t/var m)
          field
          (assoc :description (::t/field-description m))
          (assoc :isDeprecated (boolean (::t/is-deprecated m)))
          (assoc :deprecationReason (::t/deprecation-reason m)))
      {:name (name v)
       :description (::t/field-description m)
       :args []
       :type (type v)
       :isDeprecated (boolean (::t/is-deprecated m))
       :deprecationReason (::t/deprecation-reason m)})))

(defmethod field :default [v]
  (prn v)
  (throw (Exception. "field: don't know how to proceed")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- call-fn [f]
  (if (fn? f) (f) f))

(defn var->types [coll v]
  (loop [t (type v)]
    (cond
      (nil? t)  coll
      (:name t) (assoc coll (:name t) t)
      :else     (recur (:ofType t)))))

;TODO dig deeper for types

(defn type-map [schema]
  (assoc (->> schema
              vals
              (map vals)
              (reduce concat [])
              (filter var?)
              (reduce (fn [coll v] (concat coll [v] (arg-keys v) (ret-keys v))) [])
              (concat t/built-in)
              (reduce var->types {}))
         "QueryType"
         (-> base-type
             (assoc :kind t/object-kind)
             (assoc :name "QueryType")
             (assoc :description "The type that query operations will be rooted at.")
             (assoc :fields (-> schema :query vals))
             (assoc :interfaces []))
         "MutationType"
         (when (-> schema :mutation vals)
           (-> base-type
               (assoc :kind t/object-kind)
               (assoc :name "MutationType")
               (assoc :description "If this server supports mutation, the type that mutation operations will be rooted at.")
               (assoc :fields (-> schema :mutation vals))
               (assoc :interfaces [])))
         "SubscriptionType"
         (when (-> schema :subscription vals)
           (-> base-type
               (assoc :kind t/object-kind)
               (assoc :name "SubscriptionType")
               (assoc :description "If this server support subscription, the type that subscription operations will be rooted at.")
               (assoc :fields (-> schema :subscription vals))
               (assoc :interfaces [])))))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;


(defn __fields
  [{type-name :name} {all? :includeDeprecated} _ info]
  (let [field-list (map field (get-in info [:type-map type-name :fields]))]
    (if all?
      field-list
      (filter #(not (:isDeprecated %)) field-list))))

(defn __schema
  "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes
  all available types and directives on the server, as well as the entry points
  for query, mutation, and subscription operations."
  [_ _ _ info]
  {:types (map #(assoc % :fields #'__fields)  (->> info :type-map vals (remove nil?)))
   :queryType        (-> info (get-in [:type-map "QueryType"])        (assoc :fields #'__fields))
   :mutationType     (some-> info (get-in [:type-map "MutationType"])     (assoc :fields #'__fields))
   :subscriptionType (some-> info (get-in [:type-map "SubscriptionType"]) (assoc :fields #'__fields))
   :directives []}) ;TODO add directives


(defn __type
  "The fundamental unit of any GraphQL Schema is the type. There are
  many kinds of types in GraphQL as represented by the `__TypeKind` enum.

  Depending on the kind of a type, certain fields describe
  information about that type. Scalar types provide no information
  beyond a name and description, while Enum types provide their values.
  Object and Interface types provide the fields they describe. Abstract
  types, Union and Interface, provide the Object types possible
  at runtime. List and NonNull types compose other types"
  [_ {name :name} _ info]
  (some-> info
      (get-in [:type-map name])
      (assoc :fields #'__fields)))

;;;

(s/def ::name (s/nilable t/string))

(s/def ::type (s/keys :req-un [::kind ::name ::description ::fields ::interfaces ::possibleTypes ::enumValues ::inputFields ::ofType]))

(s/def ::field-node (s/keys :req-un [::name ::description ::args ::type ::isDeprecated ::deprecationReason]))

(s/def ::types (s/* ::type))

(s/def ::queryType ::type)
(s/def ::mutationType     (s/nilable ::type))
(s/def ::subscriptionType (s/nilable ::type))


(s/fdef __fields
        :args (s/tuple ::type map? map? map?)
        :ret (s/nilable (s/* ::field-node)))

(s/fdef __schema
        :args (s/tuple map? map? map? map?)
        :ret (s/keys :req-un [::types ::queryType ::mutationType ::subscriptionType ::directives]))

(s/fdef __type
        :args (s/tuple map? (s/keys :req-un [::name]) map? map?)
        :ret (s/nilable ::type))
