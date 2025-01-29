(ns test.examples.ring-jetty-handler
  (:require
    [test.examples.common :as common]
    [test.examples.counter :as counter]
    [test.examples.form :as form]
    [starfederation.datastar.clojure.adapter.ring-jetty :as jetty-gen]
    [reitit.ring :as rr]))

;; -----------------------------------------------------------------------------
;; counters
;; -----------------------------------------------------------------------------
(def update-signal (counter/->update-signal jetty-gen/->sse-response))

(defn increment
  ([req]
   (update-signal req inc))
  ([req respond _]
   (respond (update-signal req inc))))


(defn decrement
  ([req]
   (update-signal req dec))
  ([req respond _]
   (respond (update-signal req dec))))


(def counter-routes
  ["/counters/"
   ["" {:handler #'counter/counters}]
   ["increment/:id" #'increment]
   ["decrement/:id" #'decrement]])


;; -----------------------------------------------------------------------------
;; Form
;; -----------------------------------------------------------------------------
(def endpoint (form/->endpoint jetty-gen/->sse-response))


(def form-routes
  ["/form"
   ["" {:handler #'form/form}]
   ["/endpoint" {:middleware [common/wrap-mpparams]
                 :handler #'endpoint}]])


(def router
  (rr/router
    [counter-routes
     form-routes]))


(def handler
  (rr/ring-handler router
                   common/default-handler
                   {:middleware common/global-middleware}))



