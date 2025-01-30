(ns examples.multiple-fragments
  (:require
    [examples.common :as c]
    [examples.utils :as u]
    [dev.onionpancakes.chassis.core :as h]
    [reitit.ring :as rr]
    [reitit.ring.middleware.parameters :as reitit-params]
    [ring.util.response :as ruresp]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]))


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
  (h/html [(res "res-1" input-val)
           (res "res-2" input-val)]))


(defn endpoint [req]
  (let [signals (u/get-signals req)
        input-val (get signals "input")]
    (hk-gen/->sse-response req
      {:on-open
       (fn [sse]
         (d*/with-open-sse sse
          (d*/merge-fragment! sse (->fragments input-val))))})))


(def router (rr/router
              [["/" {:handler home}]
               ["/endpoint" {:handler endpoint}]]))


(def default-handler (rr/create-default-handler))


(def handler
  (rr/ring-handler router
                   default-handler
                   {:middleware [reitit-params/parameters-middleware]}))



(comment
  (u/reboot-hk-server! handler))

