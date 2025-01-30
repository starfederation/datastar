(ns test.examples.common
  (:require
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [ring.middleware.params :as rmp]
    [ring.middleware.multipart-params :as rmpp]
    [reitit.ring :as rr]))



(defn script [type src]
  [:script {:type type :src src}])


(comment
  (def datastar
    (script "module"
            "https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js")))

;; NOTE: Monitor the datastar cdn version
(def datastar
  (script "module" "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.2/bundles/datastar.js"))

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


