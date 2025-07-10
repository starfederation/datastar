(ns examples.forms.core
  (:require
    [examples.common :as c]
    [examples.forms.html :as efh]
    [examples.forms.datastar :as efd*]
    [dev.onionpancakes.chassis.core :as h]
    [examples.utils :as u]
    [ring.util.response :as rur]
    [reitit.ring :as rr]))

;; We test here several ways to manage forms, whether the plain HTML way
;; or using D*

(def home
  (h/html
    (c/page-scaffold
      [[:h1 "Forms Forms Forms"]
       [:ul
        [:li [:a {:href "/html/get"} "html GET example"]]
        [:li [:a {:href "/html/post"}"html POST example"]]
        [:li [:a {:href "/datastar/get"}"html GET example"]]
        [:li [:a {:href "/datastar/post"}"html POST example"]]]])))

(def router
  (rr/router
    [["/" {:handler (constantly (rur/response home))}]
     ["" efh/routes]
     ["" efd*/routes]
     c/datastar-route]))



(def handler
  (rr/ring-handler router (rr/create-default-handler)))

(defn after-ns-reload []
  (println "rebooting server")
  (u/reboot-hk-server! #'handler))

(comment
  (u/reboot-hk-server! #'handler))
