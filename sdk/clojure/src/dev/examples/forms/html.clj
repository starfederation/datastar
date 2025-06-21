(ns examples.forms.html
  (:require
    [examples.common :as c]
    [dev.onionpancakes.chassis.core :as h]
    [examples.utils :as u]
    [ring.util.response :as rur]
    [reitit.ring.middleware.parameters :as params]))



(defn page-get [result]
  (h/html
    (c/page-scaffold
      [:div
       [:h2 "Html GET form"]
       [:form {:action "" :method "GET"}
        [:input {:type "text"
                 :id "input1"
                 :name "input1"
                 :data-bind-input1 true}]
        [:button "submit"]]
       [:div result]])))

(defn get-home   [req]
  (let [v (get-in req [:params "input1"])]
    (u/clear-terminal!)
    (u/?req req)
    (println "got here " v)
    (rur/response (page-get v))))


(defn page-post [result]
  (h/html
    (c/page-scaffold
      [:div
       [:h2 "Html POST form !!!!"]
       [:form {:action "" :method "POST"}
        [:input {:type "text"
                 :id "input1"
                 :name "input1"
                 :data-bind-input1 true}]
        [:button "submit"]]
       [:div result]])))


(defn post-home   [req]
  (let [v (get-in req [:params "input1"])]
    (u/clear-terminal!)
    (u/?req req)
    (println "got here " v)
    (rur/response (page-post v))))



(def routes
  ["/html"
   ["/get"  {:handler #'get-home
             :middleware [params/parameters-middleware]}]
   ["/post" {:handler #'post-home
             :middleware [params/parameters-middleware]}]])



