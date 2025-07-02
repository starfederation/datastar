(ns examples.animation-gzip
  (:require
    [dev.onionpancakes.chassis.core :as h]
    [examples.animation-gzip.handlers :as handlers]
    [examples.animation-gzip.rendering :as rendering]
    [examples.animation-gzip.state :as state]
    [examples.common :as c]
    [examples.utils :as u]
    [reitit.ring :as rr]
    [reitit.ring.middleware.exception :as reitit-exception]
    [reitit.ring.middleware.parameters :as reitit-params]
    [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
    [starfederation.datastar.clojure.adapter.http-kit-schemas]
    [starfederation.datastar.clojure.adapter.ring :as ring-gen]
    [starfederation.datastar.clojure.adapter.ring-schemas]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.api-schemas]
    [starfederation.datastar.clojure.brotli :as brotli]))

;; This example let's use play with fat updates and compression
;; to get an idea of the gains compression can help use achieve
;; in terms of network usage.

(defn send-frame! [sse frame]
  (try
    (d*/patch-elements! sse frame)
    (catch Exception e
      (println e))))


(defn broadcast-new-frame! [frame]
  (let [sses @state/!conns]
    (doseq [sse sses]
      (send-frame! sse frame))))


(defn install-watch! []
  (add-watch state/!state ::watch
             (fn [_k _ref old new]
               (when-not (identical? old new)
                 (let [frame (rendering/render-content new)]
                  (broadcast-new-frame! frame))))))

(install-watch!)


(defn ->routes [->sse-response opts]
  [["/" handlers/home-handler]
   ["/ping/:id" {:handler handlers/ping-handler
                 :middleware [reitit-params/parameters-middleware]}]
   ["/random-10" handlers/random-pings-handler]
   ["/reset"     handlers/reset-handler]
   ["/step1"     handlers/step-handler]
   ["/play"      handlers/play-handler]
   ["/pause"     handlers/pause-handler]
   ["/updates"   (handlers/->updates-handler ->sse-response opts)]
   ["/refresh"   handlers/refresh-handler]
   ["/resize"    handlers/resize-handler]
   c/datastar-route])


(defn ->router [->sse-handler opts]
  (rr/router (->routes ->sse-handler opts)))


(defn ->handler [->sse-response & {:as opts}]
  (rr/ring-handler
    (->router ->sse-response opts)
    (rr/create-default-handler)
    {:middleware [reitit-exception/exception-middleware]}))


(def handler-http-kit (->handler hk-gen/->sse-response
                                 {hk-gen/write-profile (brotli/->brotli-profile)}))

(def handler-ring (->handler ring-gen/->sse-response
                             {ring-gen/write-profile ring-gen/gzip-profile}))

(defn after-ns-reload []
  (println "rebooting servers")
  (u/reboot-hk-server! #'handler-http-kit)
  (u/reboot-jetty-server! #'handler-ring {:async? true}))


(comment
  #_{:clj-kondo/ignore true}
  (user/reload!)
  :help
  :dbg
  :rec
  :stop
  *e
  state/!state
  state/!conns
  (reset! state/!conns #{})

  (-> state/!state
      deref
      rendering/page)
  (state/resize! 10 10)
  (state/resize! 20 20)
  (state/resize! 25 25)
  (state/resize! 30 30)
  (state/resize! 50 50)
  (state/reset-state!)
  (state/add-random-pings!)
  (state/step-state!)
  (state/start-animating!)
  (u/clear-terminal!)
  (u/reboot-hk-server! #'handler-http-kit)
  (u/reboot-jetty-server! #'handler-ring {:async? true}))



