(ns specialist-server.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [specialist-server.type :as t]
            [specialist-server.core :refer [server]]))

(def schema-query (-> "test/__schema.txt" io/resource slurp))

(def counter-val (atom 0))

(defn counter
  "Get current counter value"
  [node opt ctx info]
  @counter-val)

(defn inc-counter
  "Basic example mutator"
  [node opt ctx info]
  (swap! counter-val inc))

;;;

(defn happy
  "See if our greeting is happy or not."
  [node opt ctx info]
  (boolean (re-find #"!\s*$" (:greeting node))))

(defn list-field
  [node opt ctx info]
  [1 2 3 4 5 6])

(defn hello
  "Basic example resolver."
  [node opt ctx info]
  (let [n (get opt :name "world!")]
    {:greeting (str "Hello " n)
     :happy    #'happy
     :list     #'list-field}))

;;;

(s/def ::name     (s/nilable (t/field t/string "Recipient of our greeting.")))
(s/def ::greeting t/string)

(s/def ::i (t/field t/int ""))

(s/def ::happy (t/resolver #'happy))

(s/def ::list (t/resolver #'list-field))

(s/def ::hello-node (s/keys :req-un [::greeting ::happy ::list]))

(s/fdef happy
        :args (s/tuple ::hello-node map? map? map?)
        :ret t/boolean)

(s/fdef list-field
        :args (s/tuple ::hello-node map? map? map?)
        :ret (s/* ::i))

(s/fdef hello
        :args (s/tuple map? (s/keys :opt-un [::name]) map? map?)
        :ret ::hello-node)

(s/fdef counter     :args any? :ret t/int)
(s/fdef inc-counter :args any? :ret t/int)

(def schema {:query {:hello #'hello
                     :counter #'counter}
             :mutation {:counter #'inc-counter}})

(def graphql (server schema))


#_(pprint (graphql {:query schema-query}))

#_(let [query "{hello {greeting}}"
        op-name nil
        context {}
        info {:id (str (java.util.UUID/randomUUID))
              :schema schema
              :type-map {}
              :variable-values {}
              :root-value {}}]
    (pprint (execute query op-name context info)))

;;;

(deftest hello-world
  (testing "Basic queries"
    (let [res-happy (-> {:query "{hello {greeting happy}}"}               graphql :data :hello)
          res-meh   (-> {:query "{hello(name:\"meh\") {greeting happy}}"} graphql :data :hello)
          res-list  (-> {:query "{hello {list}}"} graphql :data :hello)]
      (is (= "Hello world!" (:greeting res-happy)))
      (is (= true (:happy res-happy)))
      (is (= "Hello meh" (:greeting res-meh)))
      (is (= false (:happy res-meh)))
      (is (= [1 2 3 4 5 6] (:list res-list)))))

  (testing "Query with a variable"
    (let [q {:query "query Hello($name:String) { hello(name:$name) { greeting }}"
             :variables {:name "Clojure!"}}
          res (-> q graphql :data :hello)]
      (is (= "Hello Clojure!" (:greeting res)))))

  (testing "Mutations"
    (is (= 0 (-> {:query "{counter}"} graphql :data :counter)))
    (is (= 1 (-> {:query "mutation Counter {counter}"} graphql :data :counter)))
    (is (= 2 (-> {:query "mutation Counter {counter}"} graphql :data :counter)))
    (is (= 2 (-> {:query "{counter}"} graphql :data :counter))))

  (testing "Introspection"
    (is (= {:data
            {:__type
             {:fields '({:type {:ofType {:name "String" :kind "SCALAR"}}, :name "greeting"}
                        {:type {:ofType {:name "Boolean" :kind "SCALAR"}}, :name "happy"}
                        {:type {:ofType {:name nil :kind "LIST"}}, :name "list"}
                        )}}}
           (graphql {:query "{__type(name:\"hello\") { fields { type { ofType { name kind } } name }}}"})))

    (is (= "hello" (-> {:query "{hello {__typename}}"} graphql :data :hello :__typename)))

    ))

