(ns specialist-server.batch-loader-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [specialist-server.type :as t]
            [specialist-server.batch-loader :as b]
            [specialist-server.core :refer [server]]))

(declare author)
(declare posts)
(declare comments)
(declare likes)

(def db (-> "test/blog_db.edn" io/resource slurp read-string))

(def query-counter (atom 0))

(defn db-get
  [kind id-field id-seq]
  (swap! query-counter inc)
  (if (= id-seq :all)
    (get db kind)
    (let [id-set (set id-seq)]
      (filter #(id-set (get % id-field)) (get db kind)))))

;;;

(defn author-node [v]
  (assoc v :posts #'posts))

(defn post-node [v]
  (assoc v :likes #'likes :author #'author :comments #'comments))

;;;

(def likes-batch
  (b/batch-loader
    (fn [id-set]
      (reduce
        (fn [coll v] (assoc coll (:post-id v) (:count v))) {}
        (db-get :like :post-id id-set)))))

(def comment-batch
  (b/batch-loader
    (fn [id-set]
      (reduce
        (fn [coll v] (update coll (:post-id v) conj v)) {}
        (db-get :comment :post-id id-set)))))

(def author-batch
  (b/batch-loader
    (fn [id-set]
      (let [res (db-get :author :id id-set)
            debug (reduce
                    (fn [coll v] (assoc coll (:id v) (author-node v))) {} res )]
        debug))))

(def author-posts-batch
  (b/batch-loader
    (fn [id-set]
      (reduce
        (fn [coll v] (update coll (:author-id v) conj (post-node v))) {}
        (db-get :post :author-id id-set)))))

;;;

(defmulti author (fn [[kind _] & _] kind))

(defmethod author :root [[_ node] opt ctx info]
  (some->> opt :id list (db-get :author :id) first author-node))

(defmethod author :post [[_ node] opt ctx info]
  (author-batch (:req-cache ctx) (:author-id node)))


(defmulti posts (fn [[kind _] & _] kind))

(defmethod posts :root [[_ node] opt ctx info]
  (map post-node (db-get :post :id :all)))

(defmethod posts :author [[_ node] opt ctx info]
  (author-posts-batch (:req-cache ctx) (:id node)))

(defn likes [node opt ctx info]
  (likes-batch (:req-cache ctx) (:id node)))

(defn comments [node opt ctx info]
  (comment-batch (:req-cache ctx) (:id node)))

(defn post [node opt ctx info]
  (some->> opt :id list (db-get :post :id) first post-node))

;;;

(s/def ::id        t/long)
(s/def ::post-id   t/long)
(s/def ::author-id t/long)

(s/def ::author   (t/resolver #'author))
(s/def ::comments (t/resolver #'comments))
(s/def ::posts    (t/resolver #'posts))
(s/def ::likes    (t/resolver #'likes))

(s/def ::author-node (s/keys :req-un [::id ::name ::email ::posts]))

(s/def ::post-node (s/keys :req-un [::id ::author-id ::date ::title ::text ::likes ::author ::comments]))

(s/fdef author
        :args (s/tuple (s/or :post ::post-node :root map?) (s/keys :opt-un [::id]) map? map?)
        :ret ::author-node)

(s/fdef posts
        :args (s/tuple (s/or :author ::author-node :root map?) map? map? map?)
        :ret (s/* ::post-node))

(s/fdef post
        :args (s/tuple map? (s/keys :req-un [::id]) map? map?)
        :ret ::post-node)

(s/fdef comments
        :args (s/tuple ::post-node map? map? map?)
        :ret (s/* (s/keys :req-un [::id ::post-id ::date ::text ::email])))

(s/fdef likes
        :args (s/tuple ::post-node map? map? map?)
        :ret t/int)

;;;

(def graphql (server {:query {:post   #'post
                              :posts  #'posts
                              :author #'author}}))

#_(reset! query-counter 0)

#_(pprint (graphql
            {:deferred? true :context {:req-cache (b/cache)}
             :query "{post(id:2) {__typename id title author {name} comments {text}}}" }))

#_(pprint (graphql
            {:deferred? true :context {:req-cache (b/cache)}
             :query "{posts {id title likes author {name}}}" }))

#_(pprint (graphql
            {:deferred? true :context {:req-cache (b/cache)}
             :query "{author(id:1) { posts {title comments {id text email}}}}" }))

#_(pprint (graphql {:deferred? true :context {:req-cache (b/cache)} :query "{posts {id comments {id} author {posts {comments {id}}}}}" }))


#_(time (dotimes [_ 2000]
          (graphql {:context {:req-cache (b/cache)}
                    :query "{posts {id comments {id} author {posts {comments {id}}}}}"})))


#_(deref query-counter)

;;;

(deftest batch-loader
  (testing "load comments"
    (reset! query-counter 0)
    (let [res (graphql
                {:context {:req-cache (b/cache)}
                 :query "{posts {id comments {id}}}" })]
      (is (= 2 @query-counter))
      )
    )

  #_(testing "deferred"
    (let [q "{posts {id comments {id} author {posts {comments {id}}}}}"]
      (is (=
           (graphql {:deferred? true  :context {:req-cache (b/cache)} :query q})
           (graphql {:deferred? false :context {:req-cache (b/cache)} :query q})))))

  )
