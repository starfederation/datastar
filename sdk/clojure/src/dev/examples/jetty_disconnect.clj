(ns examples.jetty-disconnect
  (:require
    [examples.utils :as u]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.ring-jetty :refer [->sse-response]]))


;; This is a small experiment to determine the behaviour of
;; ring jetty in the face of the client disconnecting

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
        {:on-open
         (fn [sse]
           (reset! !conn sse)
           (d*/console-log! sse "'connected'"))}))
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
  (d*/merge-fragment! @!conn big-message))

;; curl -vv http://localhost:8081/persistent
(comment
  (-> !conn deref d*/close-sse!)
  (send-tiny-event!)
  (d*/console-log! @!conn "'toto'")

  (def res
    (try
      (send-big-event!)
      (catch Exception e e)))
  (-> res
      (ex-cause)
      (ex-cause)
      (ex-cause)); broken pipe

  (def res2
    (try
      (send-big-event!)
      (catch Exception e e)))
  (-> res2
      (ex-cause)
      (ex-cause)
      (ex-cause))) ;closed

(comment
  (user/reload!)
  (u/clear-terminal!)
  (u/reboot-jetty-server! #'handler {:async? true}))




