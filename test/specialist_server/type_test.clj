(ns specialist-server.type-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.instant :refer [read-instant-date]]
            [clojure.pprint :refer [pprint]]
            [specialist-server.type :as t]
            [specialist-server.introspection :as i])
  (:import (java.text SimpleDateFormat)))


(t/defscalar
  url
  {:name "Url" :description "Custom url scalar."}
  (fn [v]
    (if (and (string? v) (re-find #"^https?://\w+" v))
      v
      ::s/invalid)))


(t/defscalar
  date
  {:name "Date" :description "Custom date scalar."}
  (fn [v]
    (if (inst? v)
      v
      (try
        (read-instant-date v)
        (catch Exception _ ::s/invalid)))))

(s/def ::my-url url)
(s/def ::my-date (s/nilable date))

;;;

(deftest custom-scalar
  (testing "type introspection"
    (is (= "Url" (-> ::my-url i/type :ofType :name)))
    (is (= "Custom url scalar." (-> ::my-url i/type :ofType :description))))

  (testing "conform and validate"
    (is (s/valid? ::my-url "http://example.com"))
    (is (not (s/valid? ::my-url "http://")))
    (is (not (s/valid? ::my-url "http://.")))

    (is (= "2017-11-19" (.format (SimpleDateFormat. "yyyy-MM-dd") (s/conform ::my-date "2017-11-19T10:00"))))))
