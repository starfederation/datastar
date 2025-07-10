(ns dev.examples.remove-fragments
  (:require
    [examples.common :as c]
    [examples.utils :as u]
    [dev.onionpancakes.chassis.core :as h]
    [reitit.ring :as rr]
    [reitit.ring.middleware.parameters :as reitit-params]
    [ring.util.response :as ruresp]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]))


;; Appending and removing fragments with the D* api

(def page
  (h/html
    (c/page-scaffold
      [[:h1 "Test page"]
       [:input {:type "text" :data-bind-input true :required true}]
       [:button {:data-attr-disabled "!$input"
                 :data-on-click (str (d*/sse-get "/add-fragment")
                                     "; $input = ''")}
        "Send input"]
       [:br]
       [:ul {:id "list"}]])))


(defn home [_]
  (ruresp/response page))


(defonce !counter (atom 0))


(defn id! []
  (-> !counter
      (swap-vals! inc)
      first
      (->> (str "id-"))))

(defn ->fragment [id val]
  (h/html
    [:li {:id id}
     val
     [:button {:data-on-click (d*/sse-post (str "/remove-fragment/" id))} "remove me"]]))


(defn add-element [req]
  (let [signals (u/get-signals req)
        input-val (get signals "input")]
    (hk-gen/->sse-response req
      {hk-gen/on-open
       (fn [sse]
         (d*/with-open-sse sse
          (d*/patch-elements! sse
                              (->fragment (id!) input-val)
                              {d*/selector "#list"
                               d*/patch-mode d*/pm-append})))})))


(defn remove-element [req]
  (let [id (-> req :path-params :id)]
    (hk-gen/->sse-response req
      {hk-gen/on-open
       (fn [sse-gen]
         (d*/with-open-sse sse-gen
           (d*/remove-element! sse-gen (str "#" id))))})))


(def router (rr/router
              [["/" {:handler #'home}]
               ["/add-fragment" {:handler #'add-element}]
               ["/remove-fragment/:id" {:handler #'remove-element}]
               c/datastar-route]))


(def default-handler (rr/create-default-handler))


(def handler
  (rr/ring-handler router
                   default-handler
                   {:middleware [reitit-params/parameters-middleware]}))


(comment
  (u/reboot-hk-server! handler))

