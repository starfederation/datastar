(ns examples.animation-gzip
  (:require
    [examples.animation-gzip.handlers :as handlers]
    [examples.animation-gzip.rendering :as rendering]
    [examples.animation-gzip.state :as state]
    [examples.animation-gzip.brotli :as brotli]
    [examples.utils :as u]
    [reitit.ring :as rr]
    [reitit.ring.middleware.exception :as reitit-exception]
    [reitit.ring.middleware.parameters :as reitit-params]
    [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
    [starfederation.datastar.clojure.adapter.http-kit-schemas]
    [starfederation.datastar.clojure.adapter.ring :as ring-gen]
    [starfederation.datastar.clojure.adapter.ring-schemas]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.api-schemas]))

;; This example let's use play with fat updates and compression
;; to get an idea of the gains compression can help use achieve
;; in terms of network usage.

(defn send-frame! [sse frame]
  (try
    (d*/merge-fragment! sse frame)
    (catch Exception e
      (println e))))


(defn broadcast-new-frame! [current-state]
  (let [sses @state/!conns]
    (when (seq sses)
      (let [frame (rendering/render-content current-state)]
        (doseq [sse sses]
          (send-frame! sse frame))))))


(defn install-watch! []
  (add-watch state/!state ::watch
             (fn [_k _ref old new]
               (when-not (identical? old new)
                 (broadcast-new-frame! new)))))

(install-watch!)


(defn ->routes [->sse-response opts]
  [["/" handlers/home-handler]
   ["/ping/:id" {:handler (handlers/->ping-handler ->sse-response)
                 :middleware [reitit-params/parameters-middleware]}]
   ["/random-10" (handlers/->random-pings-handler ->sse-response)]
   ["/reset"     (handlers/->reset-handler ->sse-response)]
   ["/play"      (handlers/->play-handler ->sse-response)]
   ["/pause"     (handlers/->pause-handler ->sse-response)]
   ["/updates"   (handlers/->updates-handler ->sse-response opts)]
   ["/refresh"   (handlers/->refresh-handler ->sse-response opts)]])


(defn ->router [->sse-handler opts]
  (rr/router (->routes ->sse-handler opts)))


(defn ->handler [->sse-response & {:as opts}]
  (rr/ring-handler
    (->router ->sse-response opts)
    (rr/create-default-handler)
    {:middleware [reitit-exception/exception-middleware]}))


(def handler-http-kit (->handler hk-gen/->sse-response
                                 {hk-gen/write-profile hk-gen/gzip-profile}))

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

  (state/reset-state!)
  (state/add-random-pings!)
  (state/step-state!)
  (state/start-animating!)
  (u/clear-terminal!)
  (u/reboot-hk-server! #'handler-http-kit)
  (u/reboot-jetty-server! #'handler-ring {:async? true}))



