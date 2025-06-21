(ns test.examples.form
  (:require
    [test.examples.common :as common]
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [ring.middleware.multipart-params]
    [ring.util.response :as rur]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.api :as d*]))


;; -----------------------------------------------------------------------------
;; Views
;; -----------------------------------------------------------------------------
(def input-id :input1)
(def get-button-id :get-form)
(def post-button-id :post-form)
(def form-result-id :form-result)


(defn form-get [url]
  (d*/sse-get url "{contentType: 'form'}"))


(defn form-post [url]
  (d*/sse-post url "{contentType: 'form'}"))


(defn result-area [res]
  (hc/compile
    [:span {:id form-result-id} res]))

(defn form-page []
  (common/scaffold
    (hc/compile
      [:div
       [:h2 "Form page"]
       [:form {:action ""}
        [:h3 "D* post form"]
        [:label {:for input-id} "Enter text"]
        [:br]

        [:input {:type "text"
                 :id   input-id
                 :name input-id}]
        [:br]
        [:button {:id get-button-id
                  :data-on-click (form-get "/form/endpoint")}
         "Form get"]

        [:br]
        [:button {:id post-button-id
                  :data-on-click (form-post "/form/endpoint")}
         "Form post"]


        [:br]

        (result-area "")]])))


(def page (h/html (form-page)))


(defn form
  ([_]
   (rur/response page))
  ([req respond _]
   (respond (form req))))


(defn process-endpoint [request ->sse-response]
  (let [input-val (get-in request [:params (name input-id)])]
    (->sse-response request
      {ac/on-open
       (fn [sse-gen]
         (d*/with-open-sse sse-gen
           (d*/patch-elements! sse-gen (h/html (result-area input-val)))))})))


(defn ->endpoint [->sse-response]
  (fn endpoint
    ([request]
     (process-endpoint request ->sse-response))
    ([request respond _raise]
     (respond (endpoint request)))))

