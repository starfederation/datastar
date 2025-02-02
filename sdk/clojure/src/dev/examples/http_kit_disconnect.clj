(ns examples.http-kit-disconnect
  (:require
    [examples.utils :as u]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response]]))


;; This is a small experiment to determine the behaviour of
;; ring jetty in the face of the client disconnecting

(def !conn (atom nil))

(defn long-connection [req]
  (->sse-response req
    {:on-open
     (fn [sse]
       (reset! !conn sse)
       (d*/console-log! sse "'connected'"))
     :on-close
     (fn on-close [_ status-code]
       (println "-----------------")
       (println "Connection closed status: " status-code)
       (println "-----------------"))}))


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


;; curl -vv http://localhost:8080/persistent
(comment
  (-> !conn deref d*/close-sse!)
  (send-tiny-event!)
  (d*/console-log! @!conn "'toto'"))

(comment
  (u/clear-terminal!)
  (u/reboot-hk-server! #'handler))




