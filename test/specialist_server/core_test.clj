(ns specialist-server.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [specialist-server.type :as t]
            [specialist-server.core :refer [executor]]))

(defn hello
  "Basic example resolver."
  [node opt ctx info]
  (let [n (get opt :name "world")]
    {:greeting (str "Hello " n "!")}))

(s/def ::name     (s/nilable (t/field t/string "Recipient of our greeting.")))
(s/def ::greeting t/string)

(s/fdef hello
        :args (s/tuple map? (s/keys :opt-un [::name]) map? map?)
        :ret (s/keys :req-un [::greeting]))

(def graphql (executor {:query {:hello #'hello}}))

;;;

(deftest hello-world
  (testing "Basic query"
    (is (= {:data {:hello {:greeting "Hello world!"}}}
           (graphql {:query "{hello {greeting}}"}))))

  (testing "Introspection"
    (is (= {:data {:__type {:fields '({:name "greeting"})}}}
           (graphql {:query "{__type(name:\"hello\") { fields { name }}}"})))))
