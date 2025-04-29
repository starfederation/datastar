(ns examples.broadcast-http-kit
  (:require
    [examples.utils :as u]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open on-close]]))


;; Tiny setup for that allows broadcasting events to several curl processes

(defonce !conns (atom #{}))

(defn long-connection [req]
  (->sse-response req
    {on-open
     (fn [sse]
       (swap! !conns conj sse)
       (d*/console-log! sse "'connected'"))
     on-close
     (fn on-close [sse status-code]
       (swap! !conns disj sse)
       (println "Connection closed status: " status-code)
       (println "remove connection from pool"))}))


(def routes
  [["/persistent" {:handler long-connection}]])


(def router
  (rr/router routes))


(def default-handler (rr/create-default-handler))


(def handler
  (rr/ring-handler router
                   default-handler))

(defn broadcast-number! [n]
  (doseq [conn @!conns]
    (try
      (d*/console-log! conn (str "n: " n))
      (catch Exception e
        (println "Error: " e)))))



;; open several clients:
;; curl -vv http://localhost:8080/persistent
(comment
  (-> !conns deref first d*/close-sse!)
  (broadcast-number! (rand-int 25))
  (u/clear-terminal!)
  (u/reboot-hk-server! #'handler))

