(ns test.examples.common
  (:require
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [ring.middleware.params :as rmp]
    [ring.middleware.multipart-params :as rmpp]
    [ring.util.response :as rur]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.consts :as consts]))



(defn script [type src]
  [:script {:type type :src src}])


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



(def datastar
  (script "module" "/datastar.js"))


(defn scaffold [content & {:as _}]
  (hc/compile
    [h/doctype-html5
     [:html
      [:head
        [:meta {:charset "UTF-8"}]
       datastar]
      [:body content]]]))


;; -----------------------------------------------------------------------------
;; Common Handler stuff
;; -----------------------------------------------------------------------------
(def wrap-params
  {:name ::wrap-params
   :description "Ring param extraction middleware."
   :wrap rmp/wrap-params})


(def wrap-mpparams
  {:name ::wrap-multipart-params
   :description "Ring multipart param extraction middleware."
   :wrap rmpp/wrap-multipart-params})

(def global-middleware
  [[wrap-params]])


(def default-handler
  (rr/create-default-handler))


