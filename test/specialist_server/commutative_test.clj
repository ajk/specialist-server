(ns specialist-server.commutative-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.spec.alpha :as s]
            [specialist-server.type :as t]
            [specialist-server.core :refer [executor]]))

(t/defobject A {:kind t/input-object-kind} :req-un [::type-a])
(t/defobject B {:kind t/input-object-kind} :req-un [::type-b])

(s/def ::obj-a A)
(s/def ::obj-b B)

(s/fdef a :args (s/tuple map? (s/keys :req-un [::obj-a]) map? map?) :ret t/string)
(s/fdef b :args (s/tuple map? (s/keys :req-un [::obj-b]) map? map?) :ret t/string)

(defn a [_node opt _ctx _info] (-> opt :obj-a :type-a))
(defn b [_node opt _ctx _info] (-> opt :obj-b :type-b))

(defn types [graphql]
  (->> (graphql {:query "{__schema{types{name}}}"}) :data :__schema :types
       (map :name) sort))

(deftest commutativity
  (is (= (types (executor {:query {:a #'a :b #'b}}))
         (types (executor {:query {:b #'b :a #'a}})))))

