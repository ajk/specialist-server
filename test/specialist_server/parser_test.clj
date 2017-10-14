(ns specialist-server.parser-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [specialist-server.parser :as p]))

(def query  (-> "test/introspection.txt" io/resource slurp))
(def result (-> "test/introspection.edn" io/resource slurp read-string))

;;;

(deftest antlr-test
  (testing "parse IntrospectionQuery"
    (is (= (p/graphql query) result))))
