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

(defn- apply-ops-0 [v]
  (if (seq? v)
    (cond
      (= :selectionSet (first v))        (apply str (interpose " " (rest v)))
      (= :variableDefinitions (first v)) (apply str (rest v))
      :else v)
    v))

(defn- apply-ops-1 [[_ & defs]]
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
(defn- default-value [_ v]
  v)

(defn- variable-def
  ([v-name _ v-type] (variable-def v-name ":" v-type nil))
  ([v-name _ v-type v-default]
   {v-name {:type v-type :default v-default}}))

;(:variableDefinitions "(" (:variableDefinition (:variable "$" "x") ":" (:type (:typeName "String")) (:defaultValue "=" (:value "\"X string\""))) ")")
;(:variableDefinitions "(" (:variableDefinition (:variable "$" "x") ":" (:type (:typeName "String"))) ")")
(defn- variable-defs [& v-defs]
  (apply merge (filter map? v-defs)))

(def ^:private value-or-variable identity)

(defn- variable [_ var-name]
  (keyword var-name))

(defn- value [v]
  (if (string? v)
    ; Antlr leaves double quotes in strings: "\"foo\""
    (-> v (string/replace #"^\"" "") (string/replace #"\"$" ""))
    v))

(defn- argument [k _ v] {(keyword k) v})

(defn- arguments [& args]
  {:arguments (apply merge (filter map? args))})


;(:array "[" (:value "one") "," (:value "two") "]")
(defn- array [& args]
  (->> args rest (take-nth 2)))

(defn- field-alias [a _ n]
  {:field-name (keyword n)
   :field-alias (keyword a)})

(defn- field-name [n]
  (if (map? n) ;see if already handled in field-alias
    n
    {:field-name (keyword n)}))

(defn- resolve-field-var [scalar? fun-var & args]
  ;; Run conformers for both input and output.
  (let [croak #(throw (IllegalArgumentException. "missing :args and/or :ret in fspec"))
        fspec (or (spec/get-spec fun-var) (croak))
        c-args (spec/conform (or (:args fspec) (croak)) (vec args))
        type-name (-> fun-var meta :name str)]
    (if (= c-args ::spec/invalid)
      (throw (ex-info (str "failed to conform arguments for " fun-var) (spec/explain-data (:args fspec) (vec args))))
      (let [res (apply (deref fun-var) c-args)]
        (let [res (if (fn? res) (res) res)
              c-res (spec/conform (or (:ret fspec) (croak)) res)]
          (if (= c-res ::spec/invalid)
            (do
              (println (spec/explain (:ret fspec) res)) ;DEBUG
              (throw (ex-info (str "failed to conform return value for " fun-var) (spec/explain-data (:ret fspec) res))))


            (cond
              (and scalar? (or (map? c-res) (and (coll? c-res) (-> c-res first map?))))
              (throw (IllegalArgumentException.
                       (str "invalid query on " type-name ": "
                            "the resolver returned a map or list but a scalar value was queried.")))

              (map? c-res)
              (assoc c-res :__typename type-name)

              (and (coll? c-res) (-> c-res first map?))
              (map #(assoc % :__typename type-name) c-res)

              :else c-res)
            ))))))

(defn field-args [arg-map info]
  (reduce-kv
    (fn [m k v]
      (assoc m k (if (keyword? v) (get-in info [:variable-values v]) v)))
    {} (get arg-map :arguments {})))

(defn- resolve-field [scalar? field-val-or-var arg-map root-val context info]
  (if-not (var? field-val-or-var) ;regular value or resolver var
    field-val-or-var
    (resolve-field-var scalar?
                       field-val-or-var
                       (if (seq (:path info)) root-val (:root-value info)) ;if first pass, use :root-val in info map
                       (field-args arg-map info)
                       context
                       (assoc info :field-name (:field-name arg-map)))))

(defn- resolve-field-selection [field-val-or-var arg-map root-val context info]
  (if-let [selection-set-fn (:selection-set arg-map)]
    (selection-set-fn (resolve-field false field-val-or-var arg-map root-val context info)
                      context
                      (update info :path conj (:field-name arg-map)))
    (resolve-field true field-val-or-var arg-map root-val context info)))


(defn- get-field [arg-map root-val context info]
  (if (contains? root-val (:field-name arg-map))
    (resolve-field-selection (get root-val (:field-name arg-map)) arg-map root-val context info)
    (throw (IllegalArgumentException. (str "no such field: " (:field-name arg-map) " in " (pr-str root-val))))))


(defn- field [& args]
 (apply merge-with (cons merge args)))

(def ^:private selection identity)

(defn- sel-set-field [selections context info node]
  (reduce (fn [coll s]
            (if-let [k (get s :field-alias (get s :field-name))]
              (assoc coll k (get-field s node context info))      ; regular field selection...
              (merge coll ((:selection-set s) node context info)))) ; ...or fragment selection set
          {} selections))

(defn- selection-set [& selections]
  {:selection-set (fn [node context info]
                    (let [field-fn (partial sel-set-field (filter map? selections) context info)
                          parent (if (fn? node) (node) node)] ;resolve batch loaders
                      (if (nil? parent)
                        nil
                        (if (sequential? parent)
                          (doall (map field-fn parent))
                          (field-fn parent)))))})

;;;

;TODO enforce type conditions?
(defn- frag-spread [_ name]
  {:selection-set (fn [node context info]
                    (if-let [frag (get-in info [:fragments name])]
                      ((:selection-set frag) node context info)
                      (throw (IllegalArgumentException. (str "no such fragment: " name)))))})

(def ^:private frag-name identity)

(def ^:private frag-type-cond identity)

(def ^:private frag-type-name identity)

(defn- frag-def [_ f-name _ f-type sel-set]
  {f-name (assoc sel-set :on f-type)})

;;;

(def ^:private op-type keyword)

;TODO better way to get op fn?
(defn- op-def
  ([op]      (op-def :query "default" {} op))
  ([kind op] (op-def   kind "default" {} op))
  ([kind op-name op] (op-def kind op-name {} op))
  ([kind op-name var-def op]
   (fn [schema context info]
     ;;TODO validate and combine var-defs with (:variable-values info)
     ((:selection-set op) (get schema kind)
      context
      (assoc info :operation op-name :path [])))))

(def ^:private definition identity)

(defn- document [& def-list]
  ;; Fragment definitions are maps, operations are functions.
  (let [[op-list frag-list] ((juxt filter remove) fn? def-list)
        frags (apply merge frag-list)]
    (fn [schema context info]
      ;TODO support multiple operations
      {:data ((first op-list) schema context (assoc info :fragments frags))})))

;;;

(def ^:private ops
  {:document document
   :definition definition
   :operationDefinition op-def
   :fragmentDefinition frag-def
   :fragmentName frag-name
   :fragmentSpread frag-spread
   :typeCondition frag-type-cond
   :typeName frag-type-name
   :operationType  op-type
   :fieldName field-name
   :alias field-alias
   :field field
   :selection selection
   :selectionSet selection-set
   :variableDefinition  variable-def
   :variableDefinitions variable-defs
   :defaultValue default-value
   :valueOrVariable value-or-variable
   :value value
   :variable variable
   :argument argument
   :arguments arguments
   :array array})


;;;

(defn- apply-ops [v]
  (if (and (seq? v) (keyword? (first v)))
    (apply (get ops (first v) identity) (rest v))
    v))

#_(defn parse [q-str]
  (try
    (->> q-str graphql (walk/postwalk apply-ops))
    (catch clj_antlr.ParseError ex
      (throw (IllegalArgumentException. (str "Parse error: " (.getMessage ex)))))))

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
          (throw (IllegalArgumentException. (str "Parse error: " (.getMessage ex)))))))))
