(ns examples.common
  (:require
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]))


(defn page-scaffold [body]
  (hc/compile
    [[h/doctype-html5]
     [:html
      [:head
       [:meta {:charset "UTF-8"}]
       [:script {:type "module"
                 :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.2/bundles/datastar.js"}]]
      [:body body]]]))
