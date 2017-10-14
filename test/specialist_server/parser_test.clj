(ns specialist-server.parser-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [specialist-server.parser :as p]))

(def query  (-> "test/__schema.txt" io/resource slurp))
(def result (-> "test/__schema.edn" io/resource slurp read-string))

(def bad-query
  "query ($query: String=\"query string\") {
    mutation
    subscription
    query(query:$query)
    fragment {
      query
    }
  }")

;;;

(deftest antlr-test
  (testing "parse IntrospectionQuery"
    (is (= (p/graphql query) result))
    (is (= (p/graphql-two-step query) result))
    )
  (testing "parse query with reserved words"
    (is (fn? (p/parse bad-query))))
  )
