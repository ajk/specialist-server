(ns specialist-server.introspection-test
 (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [specialist-server.type :as t]
            [specialist-server.introspection :as i]))

(defn i-resolver
  "Resolver of type Int"
  [node opt ctx info]
  1)

(defn j-resolver
  [node opt ctx info]
  {:f-enum-2 "ONE"})

(defn m-resolver
  "My type description"
  [node opt ctx info]
  {:i-resolver #'i-resolver
   :f-float 123.123})

(t/defenum my-enum "new enum type" #{"ONE" "TWO" "THREE"})

(s/def ::t-string t/string)
(s/def ::n-bool (s/nilable t/boolean))
(s/def ::c-string (s/nilable (s/* ::t-string)))

(s/def ::f-int (t/field  t/int "Int field"))
(s/def ::f-nil-int (s/nilable (t/field  t/int "Nilable int field")))

(s/def ::f-enum (t/field #{"A" "B" "C"} "My ABCs"))

(s/def ::f-enum-2 (t/field my-enum "My numbers"))

(s/def ::t-float t/float)
(s/def ::f-float (t/field (s/nilable ::t-float) "Field of type Float"))

(s/def ::composite (s/nilable (s/and ::t-string #(re-find #"foo" %))))

(s/def ::f-list (s/* ::f-int))

(s/def ::i (t/resolver #'i-resolver))
(s/def ::j (t/resolver #'j-resolver))

(t/defobject InputOne {:kind t/input-object-kind :description "Example input object"} :req-un [::t-string])

(s/def ::input-obj (t/field InputOne ""))

(s/fdef i-resolver
        :args (s/tuple map? (s/keys :req-un [::f-nil-int ::t-undef] :opt-un [::f-enum]) map? map?)
        :ret t/long)

(s/fdef j-resolver
        :args (s/tuple map? map? map? map?)
        :ret (s/keys :req-un [::f-enum-2]))

(s/def ::m-node (s/keys :req-un [::i ::j ::f-int ::missing]))

(s/def ::m-args (s/tuple map? (s/keys :req-un [::input-obj]) map? map?))

(s/fdef m-resolver
        :args ::m-args
        :ret (s/nilable (s/* ::m-node)))

;;;

#_(pprint (i/type ::f-list))
#_(pprint (i/type ::composite))

#_(pprint (s/conform ::composite "foofoo"))

#_(pprint (i/type-map {:query {:m #'m-resolver}}))

#_(pprint (keys (i/type-map {:query {:m #'m-resolver}})))

#_(pprint (i/type ::j))
#_(pprint (i/ret-keys ::j))

#_(pprint (-> ::j i/type :ofType :fields))

#_(pprint (i/field->types {} ::j))

#_(pprint (i/field #'m-resolver))

;;;

(deftest introspection-test
  (testing "type-map"
    (let [t-map (i/type-map {:query {:m #'m-resolver}})]
      (is (contains? t-map "String"))
      (is (contains? t-map "m_resolver"))
      (is (contains? t-map "my_enum"))
      (is (= {:m #'specialist-server.introspection-test/m-resolver}
             (-> t-map (get "QueryType") :fields)))))

  (testing "types"
    (is (nil? (-> ::t-string i/type :name)))
    (is (= t/non-null-kind (-> ::t-string i/type :kind)))
    (is (= t/list-kind     (-> ::c-string i/type :kind)))

    (is (= "String"  (-> ::t-string i/type :ofType :name)))
    (is (= "Boolean" (-> ::n-bool i/type :name)))
    (is (= "String"  (-> ::c-string i/type :ofType :ofType :name)))

    (is (= "String"  (-> ::composite i/type :name)))

    (is (= "Long"       (-> #'i-resolver i/type :ofType :name)))
    (is (= "j_resolver" (-> #'j-resolver i/type :ofType :name)))
    (is (= t/list-kind (-> #'m-resolver i/type :kind)))
    (is (= "m_resolver" (-> #'m-resolver i/type :ofType :ofType :name)))

    (is (set? my-enum))
    (is (= t/enum-kind (-> my-enum i/type :ofType :kind)))
    (is (= "new enum type" (-> my-enum i/type :ofType :description))))

  (testing "fields"
    (is (= "f_enum" (-> ::f-enum i/field :name)))
    (is (= "My ABCs" (-> ::f-enum i/field :description)))
    (is (= t/enum-kind (-> ::f-enum i/field :type :ofType :kind)))
    (is (= '(::i ::j ::f-int ::missing) (-> #'m-resolver i/type :ofType :ofType :fields)))

    (is (= t/enum-kind (-> ::f-enum-2 i/field :type :ofType :kind)))
    (is (= "my_enum" (-> ::f-enum-2 i/field :type :ofType :name)))

    (is (= "missing" (-> ::missing i/field :name)))
    (is (= "String"  (-> ::missing i/field :type :name))))

  (testing "input-object"
    (let [t-map (i/type-map {:query {:m #'m-resolver}})
          inp-type (-> #'m-resolver i/field :args first :type :ofType)]
      (is (contains? t-map (:name inp-type)))
      (is (= t/input-object-kind (:kind inp-type)))))

  )
