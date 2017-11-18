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

(s/def ::i-resolver (t/resolver #'i-resolver))

(s/fdef i-resolver
        :args (s/tuple map? (s/keys :req-un [::f-nil-int ::t-undef] :opt-un [::f-enum]) map? map?)
        :ret t/long)

(s/def ::m-node (s/keys :req-un [::i-resolver ::f-int ::missing ::f-enum-2]))

(s/fdef m-resolver
        :args any?
        :ret ::m-node)


;;;

#_(pprint (i/type ::f-list))
#_(pprint (i/type ::composite))

#_(pprint (s/conform ::composite "foofoo"))

#_(pprint (keys (i/type-map {:query {:m #'m-resolver}})))

;;;

(deftest introspection-test
  (testing "type-map"
    (let [t-map (i/type-map {:query {:m #'m-resolver}})]
      (is (contains? t-map "String"))
      (is (contains? t-map "m-resolver"))
      (is (contains? t-map "my-enum"))))

  (testing "types"
    (is (nil? (-> ::t-string i/type :name)))
    (is (= t/non-null-kind (-> ::t-string i/type :kind)))
    (is (= t/list-kind     (-> ::c-string i/type :kind)))

    (is (= "String"  (-> ::t-string i/type :ofType :name)))
    (is (= "Boolean" (-> ::n-bool i/type :name)))
    (is (= "String"  (-> ::c-string i/type :ofType :ofType :name)))

    (is (= "String"  (-> ::composite i/type :name)))

    (is (= "Long" (-> #'i-resolver i/type :ofType :name)))
    (is (= "m-resolver" (-> #'m-resolver i/type :ofType :name)))

    (is (set? my-enum))
    (is (= t/enum-kind (-> my-enum i/type :ofType :kind)))
    (is (= "new enum type" (-> my-enum i/type :ofType :description))))

  (testing "fields"
    (is (= "f-enum" (-> ::f-enum i/field :name)))
    (is (= "My ABCs" (-> ::f-enum i/field :description)))
    (is (= t/enum-kind (-> ::f-enum i/field :type :ofType :kind)))
    (is (= '(::i-resolver ::f-int ::missing ::f-enum-2) (-> #'m-resolver i/type :ofType :fields)))

    (is (= t/enum-kind (-> ::f-enum-2 i/field :type :ofType :kind)))
    (is (= "my-enum" (-> ::f-enum-2 i/field :type :ofType :name)))

    (is (= "missing" (-> ::missing i/field :name)))
    (is (= "String"  (-> ::missing i/field :type :name))))
  )
