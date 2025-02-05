(ns examples.common
  (:require
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [starfederation.datastar.clojure.consts :as consts]))


(def cdn-url
  (str "https://cdn.jsdelivr.net/gh/starfederation/datastar@"
       consts/version
       "/bundles/datastar.js"))


(defn page-scaffold [body]
  (hc/compile
    [[h/doctype-html5]
     [:html
      [:head
       [:meta {:charset "UTF-8"}]
       [:script {:type "module"
                 :src cdn-url}]]
      [:body body]]]))
