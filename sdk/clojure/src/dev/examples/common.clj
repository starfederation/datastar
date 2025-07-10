(ns examples.common
  (:require
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [ring.util.response :as rur]
    [starfederation.datastar.clojure.consts :as consts]))


(def cdn-url
  (str "https://cdn.jsdelivr.net/gh/starfederation/datastar@"
       consts/version
       "/bundles/datastar.js"))

(def datastar-response
  (-> (slurp "../../bundles/datastar.js")
      rur/response
      (rur/content-type "text/javascript")))


(def datastar-route
  ["/datastar.js"
   (fn ([_req]                         datastar-response)
       ([_req respond _raise] (respond datastar-response)))])


(defn page-scaffold [body]
  (hc/compile
    [[h/doctype-html5]
     [:html
      [:head
       [:meta {:charset "UTF-8"}]
       [:script {:type "module"
                 :src "/datastar.js"}]]
      [:body body]]]))

