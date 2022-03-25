(ns specialist-server.type-generator-test
  "Types from specialist-server.type can be used to generate sample data.
   This is very usefull for writing tests or for mocking resolvers while still developing.
   Another nice usecase of generated data is when you want share your code, but can't share the private/secret data that it processes."
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g]
            [specialist-server.type :as t]
            [specialist-server.core :refer [server]]
            [specialist-server.core-test :as core]))

(deftest generated-data

  (testing "Atomar datatypes from specialist-server.type"
    (is (s/valid? t/float
                  (g/generate (s/gen t/float))))
    (is (s/valid? t/long
                  (g/generate (s/gen t/long))))
    (is (s/valid? t/boolean
                  (g/generate (s/gen t/boolean))))
    (is (s/valid? t/id
                  (g/generate (s/gen t/id))))
    (is (s/valid? ::core/greeting  ;; string
                  (g/generate (s/gen ::core/greeting))))
    (is (s/valid? ::core/i  ;; field int
                  (g/generate (s/gen ::core/i))))
    (is (s/valid? ::core/name  ;; nilable field string
                  (g/generate (s/gen ::core/name)))))

  (testing "Objects (maps) can be generated too"
    (s/def ::example-return-type (s/keys :req-un [::core/greeting ::core/i ::core/name]))

    (is (s/valid? ::example-return-type
                  (g/generate (s/gen ::example-return-type))))))



(comment "Generating resolvers works, but requires the useage of s/fspec instead of s/fdef")

(s/def ::happy (s/fspec :args (s/tuple map? map? map? map?)
                        :ret boolean?
                        :gen #(g/fmap (fn [generated] (constantly generated))
                                      (s/gen boolean?))))

(s/def ::list (s/fspec :args (s/tuple map? map? map? map?)
                       :ret (s/* int?)
                       :gen #(g/fmap (fn [generated] (constantly generated))
                                     (s/gen (s/* int?)))))

(comment "Now before implementing our resolver, we can mock them with generators :)")

(def happy (g/generate (s/gen ::happy)))
(def list-field (g/generate (s/gen ::list)))

(deftest generated-resolvers
  (testing "Generated resolvers return correct types"
    (is (s/valid? (:ret (s/get-spec ::list))
                  (list-field {} {} {} {})))
    (is (s/valid? (:ret (s/get-spec ::happy))
                  (happy {} {} {} {})))))



(comment "Let's use the generated resolvers…")

(s/def ::hello-node (s/keys :req-un [::core/greeting ::happy ::list]))

(deftest generated-child-nodes
  (testing "Our resolvers can be used together with other normal implemented resolvers as child nodes"
    (is (s/valid? ::hello-node
                  (g/generate (s/gen ::hello-node))))))

(s/fdef hello
        :args (s/tuple map? (s/keys :opt-un [::core/name]) map? map?)
        :ret ::hello-node)

(comment "Child nodes must be vars. So till now, this boilerplate is required")
(s/def happy (s/get-spec ::happy))
(s/def list-field (s/get-spec ::list))

(defn hello
  "Example resolver that uses generators :)"
  [_node opt _ctx _info]
  (let [n (get opt :name "world!")]
    {:greeting (str "Hello " n)
     :happy    #'happy
     :list     #'list-field}))

(deftest generated-server
  (let [graphql (server {:query {:hello #'hello}})
        res (-> {:query "{hello {greeting happy list}}"}
                  graphql :data :hello)]

       (testing "A graphql-server with generated resolvers returns sample datasets of correct types"
         (is (s/valid? ::core/greeting
                       (-> res :greeting)))
         (is (s/valid? (:ret (s/get-spec ::happy))
                       (-> res :happy)))
         (is (s/valid? (:ret (s/get-spec ::list))
                       (-> res :list))))))
