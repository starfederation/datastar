(ns examples.jetty-disconnect
  (:require
    [examples.utils :as u]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.ring :refer [->sse-response on-open on-close]]))


;; This is a small experiment to determine the behaviour of
;; ring jetty in the face of the client disconnecting


;; 2 tiny events to detect a lost connection or 1 big event
;; Jetty internal buffer has an impact

(def !conn (atom nil))

(def big-message
  (let [b (StringBuilder.)]
    (doseq [i (range 10000)]
      (doto ^StringBuilder b
        (.append (str "-------------" i "-----------------\n"))))
    (str b)))


(defn long-connection [req respond raise]
  (try
    (respond
      (->sse-response req
        {on-open
         (fn [sse]
           (reset! !conn sse)
           (d*/console-log! sse "'connected'"))
         on-close
         (fn [_sse]
           (println "Connection lost detected")
           (reset! !conn nil))}))
    (catch Exception e
      (raise e))))


(def routes
  [["/persistent" {:handler long-connection}]])


(def router
  (rr/router routes))


(def default-handler (rr/create-default-handler))


(def handler
  (rr/ring-handler router
                   default-handler))

(defn send-tiny-event! []
  (d*/console-log! @!conn "'toto'"))


(defn send-big-event! []
  (d*/patch-elements! @!conn big-message))

;; curl -vv http://localhost:8081/persistent
(comment
  (-> !conn deref d*/close-sse!)
  (send-tiny-event!)
  (send-big-event!)

  (u/clear-terminal!)
  (u/reboot-jetty-server! #'handler {:async? true}))




