(ns examples.forms.datastar
  (:require
    [examples.common                                  :as c]
    [dev.onionpancakes.chassis.core                   :as h]
    [dev.onionpancakes.chassis.compiler               :as hc]
    [examples.utils                                   :as u]
    [ring.util.response                               :as rur]
    [reitit.ring.middleware.parameters                :as params]
    [starfederation.datastar.clojure.api              :as d*]
    [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]))




(defn result-area [res-from-form]
  (hc/compile
    [:div {:id "form-result"}
     [:span "From signals: " [:span {:data-text "$input1"}]]
     [:br]
     [:span "from backend form: " [:span  res-from-form]]]))


(defn page-get [result]
  (h/html
    (c/page-scaffold
      [:div
       [:h2 "Html GET form"]
       [:form {:action ""
               :data-on-submit "@get('/datastar/get', {contentType: 'form'})"} "submit"
        [:input {:type "text"
                 :id "input1"
                 :name "input1"
                 :data-bind-input1 true}]
        [:button "submit"]]
       (result-area result)])))


(defn get-home   [req]
  (if (not (d*/datastar-request? req))
    (-> (page-get "")
        (rur/response)
        (rur/content-type "text/html"))
    (let [v (get-in req [:params "input1"])]
      (u/clear-terminal!)
      (u/pp-request req)
      (println "got here " v)
      (->sse-response req
        {on-open
         (fn [sse]
           (d*/with-open-sse sse
             (d*/patch-elements! sse (h/html (result-area v)))))}))))


(defn page-post [result]
  (h/html
    (c/page-scaffold
      [:div
       [:h2 "Html POST form !!!!"]
       [:form {:action "" :data-on-submit "@post('/datastar/post', {contentType: 'form'})"}
        [:input {:type "text"
                 :id "input1"
                 :name "input1"
                 :data-bind-input1 true}]
        [:button "submit"]]
       (result-area result)])))


(defn post-home   [req]
  (if (not (d*/datastar-request? req))
    (rur/response (page-post ""))
    (let [v (get-in req [:params "input1"])]
      (u/clear-terminal!)
      (u/pp-request req)
      (->sse-response req
        {on-open
         (fn [sse]
           (d*/with-open-sse sse
             (d*/patch-elements! sse (h/html (result-area v)))))}))))




(def routes
  ["/datastar"
   ["/get"  {:handler #'get-home}]
             ;:middleware [params/parameters-middleware]}]
   ["/post" {:handler #'post-home
             :middleware [params/parameters-middleware]}]])



