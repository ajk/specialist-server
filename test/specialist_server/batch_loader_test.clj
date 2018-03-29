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

;;;

(deftest batch-loader
  (testing "query count"
    (reset! query-counter 0)
    (let [res (graphql
                {:context {:req-cache (b/cache)}
                 :query "{posts {id comments {id}}}" })]
      (is (= 2 @query-counter))))

  (testing "sample data 1"
    (is (= {:data
            {:post
             {:__typename "post",
              :id 2,
              :title "Omnes veniam no per",
              :author {:name "Carey Baltes"},
              :comments
              [{:text
                "Vim id fugit tation platonem, mei eu abhorreant consequuntur, te est esse latine."}]}}}
           (graphql
             {:context {:req-cache (b/cache)}
              :query "{post(id:2) {__typename id title author {name} comments {text}}}" }))))

  (testing "sample data 2"
    (is (= {:data
            {:posts
             [{:id 1,
               :title "Lorem ipsum dolor sit amet",
               :likes 2,
               :author {:name "Carey Baltes"}}
              {:id 2,
               :title "Omnes veniam no per",
               :likes 5,
               :author {:name "Carey Baltes"}}
              {:id 3,
               :title "Eam discere principes comprehensam id",
               :likes 3,
               :author {:name "Jeanna Hibbard"}}
              {:id 4,
               :title "Iuvaret incorrupte his ea",
               :likes 8,
               :author {:name "Ermelinda Mcelveen"}}]}}
           (graphql
             {:context {:req-cache (b/cache)}
              :query "{posts {id title likes author {name}}}" }))))

  (testing "sample data 3"
    (is (= {:data
            {:author
             {:name "Carey Baltes"
              :posts
              [{:title "Omnes veniam no per",
                :comments
                [{:id 3,
                  :text
                  "Vim id fugit tation platonem, mei eu abhorreant consequuntur, te est esse latine.",
                  :email "jeanna.hibbard@example.com"}]}
               {:title "Lorem ipsum dolor sit amet",
                :comments
                [{:id 7,
                  :text
                  "Laudem liberavisse pri ne, sed iisque tibique no, autem primis ancillae ut eam.",
                  :email "jack.greg@example.com"}
                 {:id 2,
                  :text
                  "Iusto fuisset expetendis qui no, cu viris exerci putent sed. Sea dolore audiam efficiantur in, in mel volutpat dissentiet.",
                  :email "jeanna.hibbard@example.com"}
                 {:id 1,
                  :text
                  "Perpetua molestiae accommodare ea has, mel eu utamur oblique.",
                  :email "ermelinda.mcelveen@example.com"}]}]}}}
           (graphql
             {:context {:req-cache (b/cache)}
              :query "{author(id:1) { name posts {title comments {id text email}}}}" }))))

  (testing "sample data 4"
    (is (= {:data
            {:posts
             [{:id 1,
               :comments [{:id 7} {:id 2} {:id 1}],
               :author
               {:name "Carey Baltes",
                :posts
                [{:comments [{:id 3}]} {:comments [{:id 7} {:id 2} {:id 1}]}]}}
              {:id 2,
               :comments [{:id 3}],
               :author
               {:name "Carey Baltes",
                :posts
                [{:comments [{:id 3}]} {:comments [{:id 7} {:id 2} {:id 1}]}]}}
              {:id 3,
               :comments [{:id 5}],
               :author {:name "Jeanna Hibbard", :posts [{:comments [{:id 5}]}]}}
              {:id 4,
               :comments [{:id 6} {:id 4}],
               :author
               {:name "Ermelinda Mcelveen",
                :posts [{:comments [{:id 6} {:id 4}]}]}}]}}
           (graphql
             {:context {:req-cache (b/cache)}
              :query "{posts {id comments {id} author {name posts {comments {id}}}}}" })))))
