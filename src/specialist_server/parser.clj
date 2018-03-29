(ns specialist-server.parser
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clj-antlr.core :as antlr]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]
            [specialist-server.type :as t]))


(def graphql (antlr/parser (slurp (io/resource "grammar/GraphQL.g4"))))

;;;

(def graphql-0 (antlr/parser (slurp (io/resource "grammar/GraphQL0.g4"))))
(def graphql-1 (antlr/parser (slurp (io/resource "grammar/GraphQL1.g4"))))

(defn apply-ops-0 [v]
  (if (seq? v)
    (cond
      (= :selectionSet (first v))        (apply str (interpose " " (rest v)))
      (= :variableDefinitions (first v)) (apply str (rest v))
      :else v)
    v))

(defn apply-ops-1 [[_ & defs]]
  `(:document
     ~@(map (fn [[_ xs]]
              `(:definition
                 ~(map (fn [x] (if (and (string? x) (re-find #"^\(|^\{" x)) (-> x graphql-1 last) x)) xs)
                 #_(~@(drop-last v) ~(-> v last graphql-1))))
            defs)))

(defn graphql-two-step
  "ANTLR graphql parser croaks if reserved words are used in selectionSet or variableDefinition:
  'query', 'mutation', 'subscription', 'fragment', 'on'

  As a workaround we can split parsing into two steps. The downside is that this is slower.
  So we try to parse queries in single pass first and if it fails we fall back to this."
  [q-str]
  (->> q-str graphql-0 (walk/postwalk apply-ops-0) apply-ops-1))

;;;

;(:defaultValue "=" (:value "\"query string\"")))
(defn default-value [_ v]
  v)

(defn variable-def
  ([v-name _ v-type] (variable-def v-name ":" v-type nil))
  ([v-name _ v-type v-default]
   {v-name {:type v-type :default v-default}}))

;(:type (:typeName "Int") (:nonNullType "!")))
(defn type-def [& args]
  (apply str args))

;(:variableDefinitions "(" (:variableDefinition (:variable "$" "x") ":" (:type (:typeName "String")) (:defaultValue "=" (:value "\"X string\""))) ")")
;(:variableDefinitions "(" (:variableDefinition (:variable "$" "x") ":" (:type (:typeName "String"))) ")")
(defn variable-defs [& v-defs]
  (apply merge (filter map? v-defs)))

(defn variable [_ var-name]
  (keyword var-name))

(defn value [v]
  (if (string? v)
    ; Antlr leaves double quotes in strings: "\"foo\""
    (-> v (string/replace #"^\"" "") (string/replace #"\"$" ""))
    v))

(defn argument [k _ v] {(keyword k) v})

(defn arguments [& args]
  (apply merge (filter map? args)))


;(:array "[" (:value "one") "," (:value "two") "]")
(defn array [& args]
  (->> args rest (take-nth 2)))

(defn field-alias [a _ n]
  (list (keyword n) (keyword a)))

(defn field-name [v]
  (if (seq? v) ;has an alias or not
    v
    (list (keyword v) (keyword v))))

(defn field
  ([field-name]
   (field field-name {} '()))
  ([field-name v]
   (if (map? v)
     (field field-name v '())
     (field field-name {} v)))
  ([field-name field-args sel-set]
   (list field-name field-args sel-set)))


(defn selection-set [& selections]
  (filter seq? selections))

;;;

(defn frag-def [_ f-name _ _ sel-set]
  (list :fragment f-name sel-set))


;;;

(defn op-def
  ([op]              (list :query "default" {} op))
  ([kind op]         (list kind   "default" {} op))
  ([kind op-name op] (list kind op-name {} op))
  ([kind op-name var-def op]
   (list kind op-name var-def op)))

(defn with-fragments [query fragment]
  (walk/postwalk
    (fn [v]
      (if (and (seq? v) (= :fragmentSpread (first v)))
        (get fragment #spy/p (last v))
        v))
    query))

(defn document [& def-list]
  (let [{:keys [query fragment]}
        (reduce (fn [coll [def-type def-name & other]]
                  (when (contains? (get coll def-type) def-name)
                    (throw (ex-info (str "Parse error: duplicate definition") {:type def-type :name def-name})))
                  (if (= :fragment def-type)
                    (assoc-in coll [:fragment def-name] other)
                    (assoc-in coll [:query def-name] (cons def-type other))))
                {:query {} :fragment {}} def-list)]
    (if (empty? fragment)
      query
      ;; Expand query fragments. Go three levels deep for now.
      (-> query
          (with-fragments fragment)
          (with-fragments fragment)
          (with-fragments fragment)))))

;;;

(def ops
  {:document document
   :operationDefinition op-def
   :definition identity
   :fragmentDefinition frag-def
   :fragmentName identity
   :typeCondition identity
   :typeName identity
   :operationType keyword
   :fieldName field-name
   :alias field-alias
   :field field
   :selectionSet selection-set
   :selection identity
   :type type-def
   :nonNullType identity
   :variableDefinition  variable-def
   :variableDefinitions variable-defs
   :defaultValue default-value
   :valueOrVariable identity
   :value value
   :variable variable
   :argument argument
   :arguments arguments
   :array array})

;;;

(defn apply-ops [v]
  (if (and (seq? v) (keyword? (first v)) (contains? ops (first v)))
    (apply (get ops (first v)) (rest v))
    v))

(defn parse [q-str]
  (try
    ;; Single-pass parsing is a bit faster so let's try it first.
    ;; This is all we need most of the time.
    (->> q-str graphql (walk/postwalk apply-ops))
    (catch clj_antlr.ParseError ex
      (try
        ;; Whoops, that didn't work.
        ;; Try to parse in two passes in order to work around reserved words in bad places.
        (->> q-str graphql-two-step (walk/postwalk apply-ops))
        (catch clj_antlr.ParseError ex
          (throw (ex-info (str "Parse error: " (.getMessage ex)) {:exception @ex})))))))
