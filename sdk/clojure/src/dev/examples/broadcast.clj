(ns examples.broadcast
  (:require
    [examples.utils :as u]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response]]))


;; This is a small experiment to determine the behaviour of
;; ring jetty in the face of the client disconnecting

(defonce !conns (atom #{}))

(defn long-connection [req]
  (->sse-response req
    {:on-open
     (fn [sse]
       (swap! !conns conj sse)
       (d*/console-log! sse "'connected'"))
     :on-close
     (fn on-close [sse status-code]
       (println "-----------------")
       (println "Connection closed status: " status-code)
       (swap! !conns disj sse)
       (println "-----------------"))}))


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
(comment)

(comment
  (-> !conns deref first d*/close-sse!)
  (broadcast-number! (rand-int 25))
  (u/clear-terminal!)
  (u/reboot-hk-server! #'handler))




