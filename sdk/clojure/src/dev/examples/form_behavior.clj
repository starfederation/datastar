(ns examples.form-behavior
  (:require
    [examples.common :as c]
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [examples.utils :as u]
    [fireworks.core :refer [?]]
    [ring.util.response :as rur]
    [reitit.ring :as rr]
    [reitit.ring.middleware.parameters :as params]
    [reitit.ring.middleware.multipart :as mpparams]
    [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]
    [starfederation.datastar.clojure.api :as d*]))


;; Trying out several way we might rightly and wrongly use html forms

(defn button [form action-fn]
  (let [method (if (= action-fn d*/sse-get)
                 "get"
                 "post")]
    (hc/compile
      [:button {:id (str method (when-not form "-no") "-form")
                :data-on-click (if form
                                 (action-fn "/endpoint" "{contentType: 'form'}")
                                 (action-fn "/endpoint"))}
       (str method (when-not form " no") " form")])))


(defn result-area [res]
  (hc/compile
    [:span {:id "form-result"} res]))


(def page
  (h/html
    (c/page-scaffold
      [:div
       [:h2 "Form page"]
       [:form {:action ""}
        [:h3 "D* post form"]
        [:label {:for "input-1"} "Enter text"]
        [:br]

        [:input {:type "text"
                 :id "input-1"
                 :name "input-1"
                 :data-bind-input-1 true}]
        [:br]
        (button false d*/sse-get)

        [:br]
        [:button {:type "button"
                  :data-on-click (d*/sse-get "/endpoint")}
         "Correct non form get"]
        [:br]
        (button true d*/sse-get)

        [:br]
        (button false d*/sse-post)

        [:br]
        (button true d*/sse-post)
        [:br]]

       [:h3 "Outside form "]
       (button false d*/sse-get)
       (button false d*/sse-post)

       [:br]
       (result-area "")])))


(defn form
  ([_]
   (rur/response page))
  ([req respond _]
   (respond (form req))))


(defn process-endpoint [request]
  (let [input-val (get-in request [:params "input-1"])
        signals (u/get-signals request)
        val (or input-val (get signals "input-1"))]
    (->sse-response request
      {on-open
       (fn [sse-gen]
         (u/clear-terminal!)
         (? (dissoc request :reitit.core/match :reitit.core/router))
         (println signals)
         (d*/with-open-sse sse-gen
           (d*/merge-fragment! sse-gen (h/html (result-area val)))))})))


(defn endpoint
    ([request]
     (process-endpoint request))
    ([request respond _raise]
     (respond (endpoint request))))


(def router
  (rr/router
    [["/" {:handler form}]
     ["/endpoint" {:handler endpoint
                   :parameters {:multipart true}
                   :middleware [mpparams/multipart-middleware]}]]))


(def handler
  (rr/ring-handler router
                   (rr/create-default-handler)
                   {:middleware [params/parameters-middleware]}))


(comment
  (u/reboot-hk-server! #'handler))
