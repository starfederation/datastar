(ns examples.tiny-gzip
  (:require
    [examples.common :as c]
    [examples.utils :as u]
    [examples.animation-gzip.brotli :as brotli]
    [dev.onionpancakes.chassis.core :as h]
    [ring.util.response :as ruresp]
    [reitit.ring :as rr]
    [reitit.ring.middleware.parameters :as reitit-params]
    [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
    [starfederation.datastar.clojure.adapter.ring :as ring-gen]
    [starfederation.datastar.clojure.adapter.common  :as ac]
    [starfederation.datastar.clojure.api :as d*]))


;; Here we try to use compression on little update to see if some
;; server buffer holds updates for short fragments.
;; It doesn't seem to be the case, compressing tiny events seems to work fine


(defonce !current-val (atom nil))
(defonce !sses (atom #{}))


(defn render-val [current-val]
  [:span#val current-val])


(defn page [current-val]
  (h/html
    (c/page-scaffold
     [:div#page {:data-on-load (d*/sse-get "/updates")}
      [:h1 "Test page"]
      [:input {:type "text"
               :data-bind-input true
                :data-on-input__debounce.100ms(d*/sse-get "/change-val")}]
      [:br]
      [:div
       (render-val current-val)]])))


(defn home
  ([_]
   (ruresp/response (page @!current-val)))
  ([req respond _]
   (respond (home req))))

(defn send-val! [sse v]
  (try
    (d*/patch-elements! sse (h/html (render-val v)))
    (catch Exception e
      (println e))))

(defn broadcast-new-val! [sses v]
  (doseq [sse sses]
    (send-val! sse v)))


(add-watch !current-val ::watch
  (fn [_key _ref _old new]
    (broadcast-new-val! @!sses new)))


(defn ->change-val [->sse-response]
  (fn change-val
    ([req]
     (let [signals (u/get-signals req)
           input-val (get signals "input")]
       (->sse-response req
         {:status 204
          ac/on-open
          (fn [sse]
            (d*/with-open-sse sse
              (reset! !current-val input-val)))})))
    ([req respond _raise]
     (respond (change-val req)))))


(defn ->updates[->sse-response opts]
  (fn updates
    ([req]
     (->sse-response req
       (merge opts
         {ac/on-open
          (fn [sse]
            (swap! !sses conj sse))
          ac/on-close
          (fn [sse & _args]
            (swap! !sses disj sse))})))
    ([req respond _raise]
     (respond (updates req)))))


(defn ->router [->sse-response opts]
  (rr/router
    [["/" {:handler home}]
     ["/change-val" {:handler (->change-val ->sse-response)
                     :middleware [reitit-params/parameters-middleware]}]
     ["/updates" {:handler (->updates ->sse-response opts)}]
     c/datastar-route]))

(def default-handler (rr/create-default-handler))


(defn ->handler [->sse-response & {:as opts}]
  (rr/ring-handler (->router ->sse-response opts)
                   default-handler))



(def handler-hk (->handler hk-gen/->sse-response
                           hk-gen/write-profile hk-gen/gzip-profile))
(def handler-ring (->handler ring-gen/->sse-response
                             ring-gen/write-profile ring-gen/gzip-profile))

(comment
  :dbg
  :rec
  (u/clear-terminal!)
  !sses
  (reset! !sses #{})
  (u/reboot-hk-server! #'handler-hk)
  (u/reboot-jetty-server! #'handler-ring {:async? true}))

