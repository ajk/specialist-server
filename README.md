# specialist-server

Wouldn't it be neat to define your entire API with [spec?](https://clojure.org/guides/spec)

GraphQL is a query language for APIs. Specialist-server is a tool for fulfilling those queries in your application. Spec is a first-class citizen here; there's not much you can do without it.

More info on GraphQL:

* [Project](http://graphql.org/)
* [Specification](http://facebook.github.io/graphql/)
* [Reference implementation](https://github.com/graphql/graphql-js)

(This project is an implementation of the specification but otherwise the author has no affiliation with creators of GraphQL.)

# Installation

[![Clojars Project](https://clojars.org/ajk/specialist-server/latest-version.svg)](https://clojars.org/ajk/specialist-server)

# Synopsis

```clojure
(ns hello-world
  (:require [clojure.spec.alpha :as s]
            [specialist-server.type :as t]
            [specialist-server.core :refer [executor]]))

;;; Resolver definitions

;; This will be included as a child node in resolver function below.
(defn happy
  "See if our greeting is happy or not."
  [node opt ctx info]
  (boolean (re-find #"!\s*$" (:greeting node))))

;; This is the (only) entry node in our graph.
(defn hello
  "Basic example resolver."
  [node opt ctx info]
  (let [n (get opt :name "world!")]
    {:greeting (str "Hello " n)
     :happy    #'happy}))

;;; Spec definitions

(s/def ::name     (s/nilable (t/field t/string "Recipient of our greeting.")))
(s/def ::greeting t/string)

(s/def ::happy (t/resolver #'happy))

(s/def ::hello-node (s/keys :req-un [::greeting ::happy]))

(s/fdef happy
        :args (s/tuple ::hello-node map? map? map?)
        :ret t/boolean)

(s/fdef hello
        :args (s/tuple map? (s/keys :opt-un [::name]) map? map?)
        :ret ::hello-node)

;;; Executor function

(def graphql (executor {:query {:hello #'hello}}))
```

Not much of a graph, but there you go. The GraphQL executor function is an excellent fit for a ring handler. A simple way to test it is to use Compojure Leiningen template:

    $ lein new compojure hello-world

Modify the project.clj file and add dependencies:

```clojure
(defproject hello-world "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.7.1"
  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [compojure "1.6.0"]
                 [ring/ring-defaults "0.3.1"]
                 [ring/ring-json "0.4.0"]
                 [ajk/specialist-server "0.3.0-SNAPSHOT"]]
  :plugins [[lein-ring "0.12.1"]]
  :ring {:handler hello-world.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.1"]]}})
```

Note that you need to use Clojure 1.9.0 for spec.
Next, create your route definitions:

```clojure
(ns hello-world.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [hello-world :as hello]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :refer [response]]))

(defroutes app-routes
  (POST "/graphql" req
        (response (hello/graphql (:body req))))
  (route/not-found "Not Found")) 
  
(def app
  (-> app-routes
   (wrap-json-response)
   (wrap-json-body {:keywords? true :bigdecimals? true})))
```

How simple is that! You can sprinkle in some middleware for auth and stuff but otherwise that's pretty much it. REST, I'm leaving you.

Let's start up your newfangled app and run some queries:

    $ lein ring server-headless

    $ curl -H 'Content-type: application/json' -d '{"query":"{ hello { greeting }}"}' http://localhost:3000/graphql
    $ curl -H 'Content-type: application/json' -d '{"query":"{ hello(name:\"Clojure!\") { greeting happy }}"}' http://localhost:3000/graphql
    
Now you're thinking with graphs!

**[Please see the wiki for full documentation.](https://github.com/ajk/specialist-server/wiki)**

# Status

Very much in alpha. You can expect breaking changes.

A good part of the GraphQL specification is done and we'll deal with the missing chunks next. Type introspection more or less works but I'm sure you can find interesting ways to break it.

# License

Copyright 2017 Antti Koskinen

Distributed under the Eclipse Public License, the same as Clojure.
