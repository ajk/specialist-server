(ns specialist-server.batch-loader
  (:require [clojure.set :refer [difference union]]))

(defn- cache-get [c cache-key]
  (get-in (deref c) cache-key))

(defn- load-batch [c cache-key fun id ctx]
  (let [cached (cache-get c [cache-key :data])]
    (if (contains? cached id)
      (get cached id)
      (let [id-set (difference (cache-get c [cache-key :id-set]) (set (keys cached)))
            default-map (zipmap id-set (repeat nil)) ; map all keys we try to find
            result (if (seq id-set) (apply fun (cons id-set ctx)) {})]
        (if (map? result)
          (-> c
              (swap! update-in [cache-key :data] merge default-map result)
              (get-in [cache-key :data id]))
          (throw (IllegalArgumentException.
                   "batch-loader must return a map.")))))))

(defn batch-loader [fun]
  (let [cache-key (str (java.util.UUID/randomUUID))]
    (fn [c id & ctx]
      (swap! c update-in [cache-key :id-set] union (-> id list set))
      (fn []
        (load-batch c cache-key fun id ctx)))))

(defn cache [] (atom {}))
