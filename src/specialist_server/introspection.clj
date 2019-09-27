(ns specialist-server.introspection
  (:refer-clojure :exclude [type])
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [specialist-server.type :as t]))

;TODO
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

(def default-type (assoc base-type :name "String"))

(def spec-ns "clojure.spec.alpha") ;TODO rename when not alpha

; set of specs we consider to be lists
(def coll-type? (->> '(* + coll-of every) (map #(symbol spec-ns (str %))) set))

;;;

(defn name-str [v]
  (when-not (nil? v) 
    (string/replace (name v) "-" "_")))

(defn even->map [coll]
  (if-not (-> coll count even?)
    (throw (IllegalArgumentException. "even->map: input list must have even count")))
  (into {} (map vec (partition 2 coll))))

(defn get-spec [v]
  (some-> v s/get-spec s/form))

(defn non-null [t]
  (-> base-type
      (assoc :kind t/non-null-kind)
      (assoc :ofType t)))

(defn field-meta [v]
  (when (seq v)
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

(defmulti arg-keys (fn [v]
                     (cond
                       (var? v)     :var
                       (keyword? v) :kw
                       (seq? v)     :keys
                       :else :default)))

(defmethod arg-keys :var [v]
  (arg-keys (-> v get-spec rest even->map :args)))

(defmethod arg-keys :kw [v]
  (let [spec (s/get-spec v)
        spec-meta (meta spec)]
    (if (::t/var spec-meta)
      (arg-keys (::t/var spec-meta))
      (some-> spec s/form arg-keys))))

(defmethod arg-keys :keys [v]
  (cond
    (= 'clojure.spec.alpha/tuple (first v))
    (-> v rest second arg-keys)

    (= 'clojure.spec.alpha/keys (first v))
    (apply concat (-> v rest even->map (select-keys [:req-un :opt-un]) vals))

    :else (arg-keys (second v))))

(defmethod arg-keys :default [v] nil)


(defn args [v]
  (let [a-keys (arg-keys v)
        a-list (map #(hash-map
                       :name (name-str %)
                       :description nil
                       :type default-type
                       :defaultValue nil) a-keys)] ;TODO support default values
    (map (fn [a b]
           (if b
             (assoc a :description (:description b) :type (:type b))
             a))
         a-list (map field a-keys))))

(defn wrap-type [v wrap]
  (let [flat-wrap (-> wrap flatten butlast reverse)]
    (reduce (fn [t w]
              (cond
                (coll-type? w) (-> base-type
                                   (assoc :kind t/list-kind)
                                   (assoc :ofType t)
                                   non-null)
                (= 'clojure.spec.alpha/nilable w)
                (if (= t/non-null-kind (:kind t)) (:ofType t) t)
                :else t))
            v flat-wrap)))

(defmulti type (fn [v]
                 (cond
                   (var? v)     :var
                   (keyword? v) :kw
                   (map? v)     :map
                   (set? v)     :set
                   (seq? v)     :list
                   (symbol? v)  :sym
                   )))

(defmethod type :var [v]
  (let [v-meta (meta v)
        v-name (get v-meta ::t/name (-> v-meta :name str))
        doc (get v-meta ::t/type-description (:doc v-meta))
        ret (-> v get-spec rest even->map :ret)
        field-keys (ret-keys ret)]
    (if field-keys
      (-> base-type
          (assoc :kind t/object-kind)
          (assoc :name (name-str v-name))
          (assoc :description doc)
          (assoc :fields field-keys) ;just the keys, break infinite type -> fields -> type loops
          (assoc :interfaces [])
          non-null
          (wrap-type ret))
      (type ret))))

(defmethod type :kw [v]
  (let [spec (s/get-spec v)
        spec-meta (meta spec)]
    (if (or (::t/var spec-meta) (::t/kind spec-meta))
      (type (assoc spec-meta :type-kw v))
      (some-> spec s/form type))))


(defmethod type :sym [v]
  (let [v-res (resolve v)
        v-meta (meta v-res)]
    (cond
      (= (::t/kind v-meta) t/enum-kind) (type (eval v)) ;; Get enum set value
      :else (type v-meta))))

(defmethod type :set [v]
  (let [m (meta v)]
    (-> base-type
        (assoc :kind t/enum-kind)
        (assoc :name (name-str (get m ::t/name (str "Enum" (hash v)))))
        (assoc :description (::t/type-description m))
        (assoc :enumValues (map (fn [e]
                                  {:name e
                                   :description nil
                                   :isDeprecated false
                                   :deprecationReason nil})
                                v))
        non-null)))

(defmethod type :list [v]
  ;;TODO test more variations
  (cond
    (coll-type? (first v))
    (-> base-type
        (assoc :kind t/list-kind)
        (assoc :ofType (type (second v)))
        non-null)

    (= 'clojure.spec.alpha/nilable (first v))
    (let [t (type (second v))]
      ;;strip non-null type wrapper
      (if (= t/non-null-kind (:kind t)) (:ofType t) t))

    (= 'clojure.spec.alpha/and (first v))
    (some #(type %) (rest v))

    (= 'specialist-server.type/field (first v))
    (let [m (field-meta v)]
      (cond
        (set? (second v))
        (type (second v))

        (symbol? (second v))
        (type (-> v second resolve deref))

        ;;example: (s/def ::foo (t/field (s/* ::bar) "Some foo list"))
        (and (seq? (second v)) (coll-type? (-> v second first)))
        (-> base-type
            (assoc :kind t/list-kind)
            (assoc :ofType (type (second v)))
            non-null)

        :else
        (-> base-type
            (assoc :kind (::t/kind m))
            (assoc :name (name-str (::t/name m)))
            (assoc :description (::t/type-description m))
            non-null)))))


(defmethod type :map [v]
  (cond
    (::t/var v)
    (type (::t/var v))

    (= t/input-object-kind (::t/kind v))
    (-> base-type
        (dissoc :fields)
        (assoc :kind t/input-object-kind)
        (assoc :name (::t/name v))
        (assoc :description (get v ::t/type-description (:doc v)))
        (assoc :inputFields (get v ::t/fields  (-> v :type-kw ret-keys))) ; Just the keys, break infinite type -> fields -> type loops
        non-null)

    (= t/object-kind (::t/kind v))
    (-> base-type
        (assoc :kind t/object-kind)
        (assoc :name (::t/name v))
        (assoc :description (get v ::t/type-description (:doc v)))
        (assoc :fields (get v ::t/fields  (-> v :type-kw ret-keys))) ; Just the keys, break infinite type -> fields -> type loops
        (assoc :interfaces [])
        non-null)

    :else
    (-> base-type
        (assoc :kind (::t/kind v))
        (assoc :name (name-str (::t/name v)))
        (assoc :description (get v ::t/type-description (:doc v)))
        non-null)))

(defmethod type :default [v]
  nil)

;;;

(defmulti field (fn [v]
                  (cond
                    (var? v)     :var
                    (keyword? v) :kw
                    (seq? v)     :list)))

(defmethod field :var [v]
  (let [v-meta (meta v)
        v-name (-> v-meta :name str)
        doc (get v-meta ::t/field-description (:doc v-meta))
        depr (:deprecated v-meta)]
    {:name (name-str v-name)
     :description doc
     :args (args v)
     :type (type v)
     :isDeprecated (boolean depr)
     :deprecationReason depr}))

(defmethod field :kw [v]
  (let [spec (s/get-spec v)
        m (or (meta spec) (some-> spec s/form field-meta))]
    (cond
      (nil? spec) ;no spec for kw => default field
      {:name (name-str v)
       :description nil
       :args []
       :type default-type
       :isDeprecated false
       :deprecationReason nil}

      (::t/var m)
      (-> (::t/var m)
          field
          (assoc :name (name-str v))
          (assoc :description (::t/field-description m))
          (assoc :isDeprecated (boolean (::t/is-deprecated m)))
          (assoc :deprecationReason (::t/deprecation-reason m)))

      (::t/field-description m)
      {:name (name-str v)
       :description (::t/field-description m)
       :args []
       :type (type v)
       :isDeprecated (boolean (::t/is-deprecated m))
       :deprecationReason (::t/deprecation-reason m)}

      :else (some-> spec s/form field (assoc :name (name-str v))))))



(defmethod field :list [v-list]
  (let [v-field (loop [v v-list]
                  (cond
                    (not (seq? v)) nil
                    (= 'specialist-server.type/field (first v)) (eval v)
                    :else (recur (second v))))
        m (meta v-field)]
    {:name (name-str (::t/field-name m))
     :description (::t/field-description m)
     :args []
     :type (type v-list)
     :isDeprecated (boolean (::t/is-deprecated m))
     :deprecationReason (::t/deprecation-reason m)}))

(defmethod field :default [v]
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- call-fn [f]
  (if (fn? f) (f) f))

(defn field->types [coll v]
  (log/trace "field->types: inspecting argument" v)
  (let [v-type (loop [v-t (type v)]
                 (cond
                   (nil? v-t)  nil
                   (:name v-t) v-t
                   :else (recur (:ofType v-t))))
        v-fields (or
                   (-> v-type :fields)
                   (-> v-type :ofType :fields)
                   (-> v-type :ofType :ofType :fields)
                   '())
        v-input-fields (or
                         (-> v-type :inputFields)
                         (-> v-type :ofType :inputFields)
                         (-> v-type :ofType :ofType :inputFields)
                         '())]
    (log/trace "-> got type:" v-type)
    (cond
      (nil? v-type) coll
      (contains? coll (:name v-type)) coll
      :else
      (reduce field->types
              (assoc coll (:name v-type) v-type)
              (set (concat v-input-fields v-fields (arg-keys v) (ret-keys v)))))))

(defn type-map [schema]
  (assoc (->> schema
              vals
              (map vals)
              (reduce into [])
              (filter var?)
              (into  t/built-in)
              (reduce field->types {}))
         "QueryType"
         (-> base-type
             (assoc :kind t/object-kind)
             (assoc :name "QueryType")
             (assoc :description "The type that query operations will be rooted at.")
             (assoc :fields (:query schema))
             (assoc :interfaces []))
         "MutationType"
         (when (-> schema :mutation vals)
           (-> base-type
               (assoc :kind t/object-kind)
               (assoc :name "MutationType")
               (assoc :description "If this server supports mutation, the type that mutation operations will be rooted at.")
               (assoc :fields (:mutation schema))
               (assoc :interfaces [])))
         "SubscriptionType"
         (when (-> schema :subscription vals)
           (-> base-type
               (assoc :kind t/object-kind)
               (assoc :name "SubscriptionType")
               (assoc :description "If this server support subscription, the type that subscription operations will be rooted at.")
               (assoc :fields (:subscription schema))
               (assoc :interfaces [])))))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

(declare with-field-resolver)

(defn- field-node [field-list all?]
  (let [out-list (map (fn [v]
                        (-> (if (vector? v)
                              ;; If we are looping over a map, use key as field name
                              (-> v last field (assoc :name (-> v first name)))
                              (field v))
                            (assoc :defaultValue nil) ;TODO support default values on inputValues
                            (update :type with-field-resolver)))
                      field-list)]
    (if all?
      out-list
      (filter #(not (:isDeprecated %)) out-list))))

(defmulti __fields (fn [[kind _] _ _ _] kind))

(defmethod  __fields :object
  [[_ node] {all? :includeDeprecated} _ info]
  (field-node (-> info (get-in [:type-map (:name node)]) :fields) all?))

(defmethod __fields  :input
  [[_ node] {all? :includeDeprecated} _ info]
  (field-node (-> info (get-in [:type-map (:name node)]) :inputFields) all?))

(defmethod __fields :default
  [_ _ _ _]
  nil)

(defn- with-field-resolver
  "Adds field resolver ref to type map v"
  [v]
  (cond
    (or (= t/non-null-kind (:kind v)) (= t/list-kind (:kind v)))
    (update v :ofType with-field-resolver)

    (= t/input-object-kind (:kind v))
    (assoc v :inputFields #'__fields :fields nil)

    :else
    (assoc v :fields #'__fields :inputFields nil)))

(defn __schema
  "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes
  all available types and directives on the server, as well as the entry points
  for query, mutation, and subscription operations."
  [_ _ _ info]
  {:types (map with-field-resolver  (->> info :type-map vals (remove nil?)))
   :queryType        (-> info (get-in [:type-map "QueryType"])            with-field-resolver)
   :mutationType     (some-> info (get-in [:type-map "MutationType"])     with-field-resolver)
   :subscriptionType (some-> info (get-in [:type-map "SubscriptionType"]) with-field-resolver)
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
  (if-let [v (get-in info [:type-map name])]
    (with-field-resolver v)))

;;;

(s/def ::kind #{t/scalar-kind t/object-kind t/interface-kind t/union-kind t/enum-kind t/input-object-kind t/list-kind t/non-null-kind})

(s/def ::name t/string)

(s/def ::description (s/nilable t/string))

(s/def ::type (s/or :non-null (s/and #(= (:kind %) t/non-null-kind)     (s/keys :req-un [::kind ::ofType]))
                    :list     (s/and #(= (:kind %) t/list-kind)         (s/keys :req-un [::kind ::ofType]))
                    :object   (s/and #(= (:kind %) t/object-kind)       (s/keys :req-un [::kind ::name ::description ::fields ::interfaces]))
                    :input    (s/and #(= (:kind %) t/input-object-kind) (s/keys :req-un [::kind ::name ::description ::inputFields]))
                    :scalar   (s/and #(= (:kind %) t/scalar-kind)       (s/keys :req-un [::kind ::name ::description]))
                    :enum     (s/and #(= (:kind %) t/enum-kind)         (s/keys :req-un [::kind ::name ::description ::enumValues]))))

(s/def ::ofType ::type)

(s/def ::defaultValue (s/nilable string?))

(s/def ::inputValue (s/keys :req-un [::name ::description ::type ::defaultValue]))

(s/def ::isDeprecated boolean?)

(s/def ::deprecationReason (s/nilable string?))

(s/def ::enumValue (s/keys :req-un [::name ::description ::isDeprecated ::deprecationReason]))

(s/def ::enumValues (s/nilable (s/+ ::enumValue)))

(s/def ::args (s/* ::inputValue))


(s/def ::field-node (s/or :field (s/keys :req-un [::name ::description ::type ::isDeprecated ::deprecationReason ::defaultValue])
                          :input ::inputValue))

(s/def ::types (s/* ::type))


(s/def ::fields      (t/resolver #'__fields ""))
(s/def ::inputFields (t/resolver #'__fields ""))

(s/def ::queryType ::type)
(s/def ::mutationType     (s/nilable ::type))
(s/def ::subscriptionType (s/nilable ::type))


(s/fdef __fields
        :args (s/tuple ::type map? map? map?)
        :ret (s/nilable (s/+ ::field-node)))

(s/fdef __schema
        :args (s/tuple map? map? map? map?)
        :ret (s/keys :req-un [::types ::queryType ::mutationType ::subscriptionType ::directives]))

(s/fdef __type
        :args (s/tuple map? (s/keys :req-un [::name]) map? map?)
        :ret (s/nilable ::type))
