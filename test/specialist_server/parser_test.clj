(ns specialist-server.parser-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
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


#_(pprint (p/parse "query Foo($a: String!=\"?\") {foo(a:1) {bar baz(b:2)} foo2 {bar2 baz2}}"))

#_(pprint (p/parse "query Foo { foo {bar {...Frag} baz} } query Bar {bar ...Frag } fragment Frag on bar {quux quux2}"))

#_(pprint (p/parse "query Foo { foo {bar {...Frag meh} baz} } fragment Frag on bar {quux quux2}"))

#_(pprint (p/parse query))

#_(time (dotimes [_ 1000]
          (p/parse query)))

;;;

(deftest antlr-test
  (testing "parse IntrospectionQuery"
    (is (= (p/graphql query) result))
    (is (= (p/graphql-two-step query) result))
    )
  (testing "parse query with reserved words"
    (is (map? (p/parse bad-query))))
  )
