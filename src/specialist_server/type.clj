(ns specialist-server.type
  (:refer-clojure :exclude [int long float boolean])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]))

(alias 't 'specialist-server.type)

(def scalar-kind "SCALAR")
(def object-kind "OBJECT")
(def interface-kind "INTERFACE")
(def union-kind "UNION")
(def enum-kind "ENUM")
(def input-object-kind "INPUT_OBJECT")
(def list-kind "LIST")
(def non-null-kind "NON_NULL")


(defmacro defscalar
  "Defines new scalar types."
  [type-name type-meta conform-fn]
  (let [meta-map (if (map? type-meta) type-meta {:name (name type-name) :description type-meta})]
    `(def ~type-name
       (vary-meta (s/conformer ~conform-fn)
                  assoc
                  :specialist-server.type/name ~(:name meta-map)
                  :specialist-server.type/kind ~scalar-kind
                  :specialist-server.type/type-description ~(:description meta-map)
                  :specialist-server.type/field-description "Self descriptive."))))


(defn field
  ([t doc] (field t doc {}))
  ([t doc opt]
   (vary-meta t
              assoc
              :specialist-server.type/field-description doc
              :specialist-server.type/is-deprecated (clojure.core/boolean (:deprecated opt))
              :specialist-server.type/deprecation-reason (:deprecated opt))))

;;;

(defscalar
  string
  {:name "String"
   :description
   (str "The 'String' scalar type represents textual data, represented as UTF-8 "
        "character sequences. The String type is most often used by GraphQL to "
        "represent free-form human-readable text.")}
  (fn [v]
    (cond
      (nil? v) nil
      (coll? v) ::s/invalid
      :else (str v))))

(defscalar
  int
  {:name "Int"
   :description
   (str "The 'Int' scalar type represents non-fractional signed whole numeric values. "
        "Int can represent values between -(2^31) and 2^31 - 1.")}
  (fn [v]
    (if (clojure.core/int? v)
      v
      (try (Integer. ^String v) (catch Exception _ ::s/invalid)))))

(defscalar
  long
  {:name "Long"
   :description
   (str "The 'Long' scalar type represents non-fractional signed whole numeric "
        "values. Long can represent values between -(2^64) and 2^64 - 1.")}
  (fn [v]
    (if (clojure.core/integer? v)
      v
      (try (Long. ^String v) (catch Exception _ ::s/invalid)))))

(defscalar
  float
  {:name "Float"
   :description
   (str "The 'Float' scalar type represents signed double-precision fractional values "
        "as specified by IEEE 754")}
  (fn [v]
    (if (clojure.core/float? v)
      v
      (try (Double. ^String v) (catch Exception _ ::s/invalid)))))

(defscalar
  boolean
  {:name "Boolean"
   :description "The 'Boolean' scalar type represents 'true' or 'false'."}
  (fn [v]
    (cond
      (string/blank? v) ::s/invalid
      (= true v)  true
      (= false v) false
      (and (clojure.core/string? v) (re-find #"(?i)^true$" v)) true
      (and (clojure.core/string? v) (re-find #"(?i)^false$" v)) false
      :else ::s/invalid)))

(defscalar
  id
  {:name "ID"
   :description
   (str "The 'ID' scalar type represents a unique identifier, often used to refetch "
        "an object or as key for a cache. The ID type appears in a JSON response as a "
        "String; however, it is not intended to be human-readable. When expected as an "
        "input type, any string (such as \"4\") or integer (such as 4) input value "
        "will be accepted as an ID.")}
  (fn [v]
    (cond
      (string/blank? v) ::s/invalid
      (coll? v) ::s/invalid
      :else (str v))))

(defn resolver
  "The 'resolver' scalar type represents references to other resolver functions.
  These are used for connecting nodes in the graph. It doesn't represent a type
  in itself."
  ([we-have] (resolver we-have "Self descriptive."))
  ([we-have doc] (resolver we-have doc {}))
  ([we-have doc opt]
   (vary-meta
     (s/conformer
       (fn [they-sent]
         (if (and (var? they-sent) (= we-have they-sent))
           they-sent
           ::s/invalid)))
     assoc ::t/var we-have
           ::t/field-description doc
           ::t/is-deprecated (clojure.core/boolean (:deprecated opt))
           ::t/deprecation-reason (:deprecated opt))))

;;;

(def built-in (map meta [string int long float boolean id]))
