(ns examples.broadcast-ring
  (:require
    [examples.utils :as u]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.ring :refer [->sse-response]]))


(defonce !conns (atom #{}))


(defn long-connection
  ([req respond _raise]
   (respond
     (->sse-response req
       {:on-open
        (fn [sse]
          (swap! !conns conj sse)
          (try
            (d*/console-log! sse "'connected with jetty!'")
            (catch Exception _
              (d*/close-sse! sse))))
        :on-close
        (fn on-close [sse]
          (swap! !conns disj sse)
          (println "Removed connection from pool"))}))))


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
      (catch Exception _
        (d*/close-sse! conn)
        (println "closing connection")))))



;; open several clients:
;; curl -vv http://localhost:8081/persistent
(comment
  (-> !conns deref first d*/close-sse!)
  (reset! !conns #{})
  (broadcast-number! (rand-int 25))
  (u/clear-terminal!)
  (u/reboot-jetty-server! #'handler {:async? true})
  (u/reboot-rj9a-server! #'handler {:async? true}))

