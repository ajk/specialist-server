(ns specialist-server.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [specialist-server.type :as t]
            [specialist-server.core :refer [executor]]))

(defn happy
  "See if our greeting is happy or not."
  [node opt ctx info]
  (boolean (re-find #"!\s*$" (:greeting node))))

(defn hello
  "Basic example resolver."
  [node opt ctx info]
  (let [n (get opt :name "world!")]
    {:greeting (str "Hello " n)
     :happy    #'happy}))

;;;

(s/def ::name     (s/nilable (t/field t/string "Recipient of our greeting.")))
(s/def ::greeting t/string)

(s/def ::happy (t/resolver #'happy))

(s/def ::hello-node (s/keys :req-un [::greeting ::happy]))


(s/fdef happy
        :args (s/tuple ::hello-node map? map? map?)
        :ret boolean?)

(s/fdef hello
        :args (s/tuple map? (s/keys :opt-un [::name]) map? map?)
        :ret ::hello-node)


(def graphql (executor {:query {:hello #'hello}}))

;;;

(deftest hello-world
  (testing "Basic queries"
    (let [res-happy (-> {:query "{hello {greeting happy}}"}               graphql :data :hello)
          res-meh   (-> {:query "{hello(name:\"meh\") {greeting happy}}"} graphql :data :hello)]
      (is (= "Hello world!" (:greeting res-happy)))
      (is (= true (:happy res-happy)))
      (is (= "Hello meh" (:greeting res-meh)))
      (is (= false (:happy res-meh)))))

  (testing "Introspection"
    (is (= {:data {:__type {:fields '({:name "greeting"} {:name "happy"})}}}
           (graphql {:query "{__type(name:\"hello\") { fields { name }}}"})))))
