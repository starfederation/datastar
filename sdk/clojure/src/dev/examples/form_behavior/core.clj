(ns examples.form-behavior.core
  (:require
    [clojure.java.io :as io]
    [examples.common :as c]
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [examples.utils :as u]
    [ring.util.response :as rur]
    [reitit.ring :as rr]
    [reitit.ring.middleware.parameters :as params]
    [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]
    [starfederation.datastar.clojure.api :as d*]))


;; Trying out several way we might rightly and wrongly use html forms
;; TODO: still need to figure out some things

(defn result-area [value]
  (hc/compile
    [:div {:id "form-result"}
     [:span "From signals: " [:span {:data-text "$input1"}]]
     [:br]
     [:span "from backend: " [:span (str value "- " (random-uuid))]]]))

(def style (slurp (io/resource "examples/form_behavior/style.css")))

(def page
  (h/html
    (c/page-scaffold
      [:div
       [:style style]
       [:h2 "Form page"]
       [:form {:action ""}
        [:h3 "D* post form"]
        [:label {:for "input1"} "Enter text"]
        [:br]

        [:input {:type "text"
                 :id "input1"
                 :name "input-1"
                 :data-bind-input1 true}]


        [:br]
        (result-area "")

        [:h3 "Inside form"]
        [:div#buttons
         ;; GET
         [:div
          [:h4 "GET"]
          [:button {               :data-on-click "@get('/endpoint')"}                        "get - no ct - no type"][:br]
          [:button {:type "button" :data-on-click "@get('/endpoint')"}                        "get - no ct - type"][:br]
          [:button {               :data-on-click "@get('/endpoint', {contentType: 'form'})"} "get - ct    - no type "][:br]
          [:button {:type "button" :data-on-click "@get('/endpoint', {contentType: 'form'})"} "get - ct    -type"][:br]]

          ;; POST
         [:div
          [:h4 "POST"]
          [:button {               :data-on-click "@post('/endpoint')"}                        "post - no ct - no type"][:br]
          [:button {:type "button" :data-on-click "@post('/endpoint')"}                        "post - no ct - type"][:br]
          [:button {               :data-on-click "@post('/endpoint', {contentType: 'form'})"} "post - ct    - no type "][:br]
          [:button {:type "button" :data-on-click "@post('/endpoint', {contentType: 'form'})"} "post - ct    -type"][:br]]

         ;; PUT
         [:div
          [:h4 "PUT"]
          [:button {               :data-on-click "@put('/endpoint')"}                        "put - no ct - no type"][:br]
          [:button {:type "button" :data-on-click "@put('/endpoint')"}                        "put - no ct - type"][:br]
          [:button {               :data-on-click "@put('/endpoint', {contentType: 'form'})"} "put - ct    - no type "][:br]
          [:button {:type "button" :data-on-click "@put('/endpoint', {contentType: 'form'})"} "put - ct    -type"][:br]]

         ;; PATCH
         [:div
          [:h4 "PATCH"]
          [:button {               :data-on-click "@patch('/endpoint')"}                        "patch - no ct - no type"][:br]
          [:button {:type "button" :data-on-click "@patch('/endpoint')"}                        "patch - no ct - type"][:br]
          [:button {               :data-on-click "@patch('/endpoint', {contentType: 'form'})"} "patch - ct    - no type "][:br]
          [:button {:type "button" :data-on-click "@patch('/endpoint', {contentType: 'form'})"} "patch - ct    -type"][:br]]

         ;; DELETE
         [:div
          [:h4 "DELETE"]
          [:button {               :data-on-click "@delete('/endpoint')"}                        "delete - no ct - no type"][:br]
          [:button {:type "button" :data-on-click "@delete('/endpoint')"}                        "delete - no ct - type"][:br]
          [:button {               :data-on-click "@delete('/endpoint', {contentType: 'form'})"} "delete - ct    - no type "][:br]
          [:button {:type "button" :data-on-click "@delete('/endpoint', {contentType: 'form'})"} "delete - ct    -type"][:br]]]]
       [:h3 "Outside form "]
       [:button
        {:id "get-no-form", :data-on-click "@get('/endpoint')"}
        "get no form"]
       [:button
        {:id "post-no-form", :data-on-click "@post('/endpoint')"}
        "post no form"]

       [:br]])))


(defn form
  ([_]
   (rur/response page))
  ([req respond _]
   (respond (form req))))


(defn process-form
  ([request]
   (let [value (or (some->> (get-in request [:params "input-1"])
                            (str "Form value - "))
                   (some-> request
                           u/get-signals
                           (get "input1")
                           (->> (str "Signal value - "))))]
     (u/clear-terminal!)
     (u/pp-request request)
     (println "value" value)
     (->sse-response request
       {on-open
        (fn [sse-gen]
          (d*/with-open-sse sse-gen
            (d*/patch-elements! sse-gen (h/html (result-area value)))))})))
  ([request respond _raise]
   (respond (process-form request))))


(def router
  (rr/router
    [["/" {:handler form}]
     ["/endpoint"    {:handler process-form}]
                      ;:parameters {:multipart true} :middleware [mpparams/multipart-middleware]}]
     c/datastar-route]))


(def handler
  (rr/ring-handler router
                   (rr/create-default-handler)
                   {:middleware [params/parameters-middleware]}))


(comment
  (u/reboot-hk-server! #'handler))
