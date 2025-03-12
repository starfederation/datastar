(ns examples.multiple-fragments
  (:require
    [examples.common :as c]
    [examples.utils :as u]
    [dev.onionpancakes.chassis.core :as h]
    [reitit.ring :as rr]
    [reitit.ring.middleware.parameters :as reitit-params]
    [ring.util.response :as ruresp]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
    [starfederation.datastar.clojure.adapter.ring :as ring-gen]))


;; Testing the sending of multiple fragments at once

(defn res [id val]
  [:span {:id id} val])


(def page
  (h/html
    (c/page-scaffold
     [[:h1 "Test page"]
      [:input {:type "text" :data-bind-input true}]
      [:button {:data-on-click (d*/sse-get "/endpoint")} "Send input"]
      [:br]
      [:div "res: " (res "res-1" "")]
      [:div "duplicate res: " (res "res-2" "")]])))


(defn home [_]
  (ruresp/response page))


(defn ->fragments [input-val]
  [(h/html (res "res-1" input-val))
   (h/html (res "res-2" input-val))])


(defn ->endpoint[->sse-response]
  (fn [req]
   (let [signals (u/get-signals req)
         input-val (get signals "input")]
     (->sse-response req
       {ac/on-open
        (fn [sse]
          (d*/with-open-sse sse
            (d*/merge-fragments! sse (->fragments input-val))))}))))


(defn ->router [->sse-response]
  (rr/router
    [["/" {:handler home}]
     ["/endpoint" {:handler (->endpoint ->sse-response)}]]))


(def default-handler (rr/create-default-handler))


(defn ->handler [->sse-response]
  (rr/ring-handler (->router ->sse-response)
                   default-handler
                   {:middleware [reitit-params/parameters-middleware]}))



(def handler-hk (->handler hk-gen/->sse-response))
(def handler-ring (->handler ring-gen/->sse-response))


(comment
  :dbg
  :rec
  (u/clear-terminal!)
  (u/reboot-hk-server! handler-hk)
  (u/reboot-rj9a-server! #'handler-ring))

