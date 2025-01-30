(ns test.examples.form
  (:require
    [test.examples.common :as common]
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [ring.middleware.multipart-params]
    [ring.util.response :as rur]
    [starfederation.datastar.clojure.api :as d*]))


;; -----------------------------------------------------------------------------
;; Views
;; -----------------------------------------------------------------------------
(defn form-get [url]
  (d*/sse-get url "{contentType: 'form'}"))


(defn form-post [url]
  (d*/sse-post url "{contentType: 'form'}"))


(defn result-area [res]
  (hc/compile
    [:span {:id "form-result"} res]))


(defn form-page []
  (common/scaffold
    (hc/compile
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
        [:button {:id "get-form"
                  :data-on-click (form-get "/form/endpoint")}
         "Form get"]

        [:br]
        [:button {:id "post-form"
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
  (let [input-val (get-in request [:params "input-1"])]
    (->sse-response request
      {:on-open
       (fn [sse-gen]
         (d*/with-open-sse sse-gen
           (d*/merge-fragment! sse-gen (h/html (result-area input-val)))))})))


(defn ->endpoint [->sse-response]
  (fn endpoint
    ([request]
     (process-endpoint request ->sse-response))
    ([request respond _raise]
     (respond (endpoint request)))))

