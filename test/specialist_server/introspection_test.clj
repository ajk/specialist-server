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


(s/def ::t-string t/string)
(s/def ::n-bool (s/nilable t/boolean))
(s/def ::c-string (s/nilable (s/* ::t-string)))

(s/def ::f-enum (t/field #{"A" "B" "C"} "My ABCs"))

(meta (t/field #{"A" "B" "C"} "My ABCs"))

(s/def ::t-float t/float)
(s/def ::f-float (t/field (s/nilable ::t-float) "Field of type Int"))


(s/def ::i-resolver (t/resolver #'i-resolver))

(s/fdef i-resolver
        :args (s/tuple map? (s/keys :req-un [::t-int]) map? map?)
        :ret t/long)

(s/def ::m-node (s/keys :req-un [::i-resolver ::f-int]))

(s/fdef m-resolver
        :args any?
        :ret ::m-node)

;;;

(deftest introspection-test
  (testing "types"
    (is (nil? (-> ::t-string i/type :name)))
    (is (= t/non-null-kind (-> ::t-string i/type :kind)))
    (is (= t/list-kind     (-> ::c-string i/type :kind)))

    (is (= "String"  (-> ::t-string i/type :ofType :name)))
    (is (= "Boolean" (-> ::n-bool i/type :name)))
    (is (= "String"  (-> ::c-string i/type :ofType :ofType :name)))

    ;(is (= t/enum-kind (-> ::t-enum i/type :ofType :kind)))

    (is (= "Long" (-> #'i-resolver i/type :ofType :name)))
    (is (= "m-resolver" (-> #'m-resolver i/type :ofType :name))))

  (testing "fields"
    (is (= "f-enum" (-> ::f-enum i/field :name)))
    (is (= "My ABCs" (-> ::f-enum i/field :description)))
    (is (= t/enum-kind (-> ::f-enum i/field :type :ofType :kind))))
  )