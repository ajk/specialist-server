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

(defn- value [v]
  (if (string? v)
    ; Antlr leaves double quotes in strings: "\"foo\""
    (-> v (string/replace #"^\"" "") (string/replace #"\"$" ""))
    v))

(defn- variable [_ var-name]
  (let [kw (keyword var-name)]
    (fn [node context info]
      (get-in info [:variable-values kw]))))

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
        c-args (spec/conform (or (:args fspec) (croak)) (vec args))]
    (if (= c-args ::spec/invalid)
      (throw (ex-info (str "failed to conform arguments for " fun-var) (spec/explain-data (:args fspec) (vec args))))
      (let [res (apply (deref fun-var) c-args)]
        ;; return a function now and resolve later so batch-loader has a change to collect more ids
        (fn []
          (let [res (if (fn? res) (res) res)
                c-res (spec/conform (or (:ret fspec) (croak)) res)]
            (if (= c-res ::spec/invalid)
              (do
                (println (spec/explain (:ret fspec) res)) ;DEBUG
                (throw (ex-info (str "failed to conform return value for " fun-var) (spec/explain-data (:ret fspec) res))))
              (if (and scalar? (or (map? c-res) (and (coll? c-res) (-> c-res first map?))))
                ;; the resolver returned a map but we expected a scalar value
                (throw (IllegalArgumentException. "invalid query: missing selection set")) ;TODO dig up field name
                c-res))))))))

(defn- resolve-field [scalar? field-val-or-var arg-map root-val context info]
  (if-not (var? field-val-or-var) ;regular value or resolver var
    field-val-or-var
    (resolve-field-var scalar?
                       field-val-or-var
                       (if (seq (:path info)) root-val (:root-value info)) ;if first pass, use :root-val in info map
                       (get arg-map :arguments {})
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
  ([op]      (op-def :query "default" op))
  ([kind op] (op-def   kind "default" op))
  ([kind op-name op]
   (fn [schema context info]
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

(def ^:private ops {:document document
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
          :valueOrVariable identity
          :value value
          :variable variable
          :argument argument
          :arguments arguments
          :array array})


(defn- apply-ops [v]
  (if (and (seq? v) (keyword? (first v)))
    (apply (get ops (first v) identity) (rest v))
    v))

;;;

(defn parse [q-str]
  (try
    (->> q-str graphql (walk/postwalk apply-ops))
    (catch clj_antlr.ParseError ex
      (throw (IllegalArgumentException. (str "Parse error: " (.getMessage ex)))))))
