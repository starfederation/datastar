(ns examples.scripts
  (:require
    [examples.common :as c]
    [examples.utils :as u]
    [dev.onionpancakes.chassis.core :as h]
    [reitit.ring :as rr]
    [reitit.ring.middleware.parameters :as reitit-params]
    [ring.util.response :as ruresp]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]))

;; Sending scripts and playing with auto-remove

(def page
  (h/html
    (c/page-scaffold
      [[:h1 "Test page"]
       [:button {:data-on-click (d*/sse-get "/endpoint")}
        "Say hello!"]])))


(defn home [_]
  (ruresp/response page))


(defn endpoint [req]
  (hk-gen/->sse-response req
    {hk-gen/on-open
     (fn [sse]
       (d*/with-open-sse sse
         (d*/execute-script! sse
                             "console.log('hello')"
                             {d*/auto-remove false})))}))


(def router (rr/router
              [["/" {:handler home}]
               ["/endpoint" {:handler endpoint}]]))


(def default-handler (rr/create-default-handler))


(def handler
  (rr/ring-handler router
                   default-handler
                   {:middleware [reitit-params/parameters-middleware]}))


(comment
  (u/reboot-hk-server! #'handler))
