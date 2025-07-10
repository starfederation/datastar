(ns test.examples.http-kit-handler2
  (:require
    [test.examples.common :as common]
    [test.examples.counter :as counters]
    [test.examples.form :as form]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.adapter.http-kit2 :as hk-gen]))


;; -----------------------------------------------------------------------------
;; Counters
;; -----------------------------------------------------------------------------
(def update-signal (counters/->update-signal hk-gen/->sse-response))


(defn increment
  ([req]
   (update-signal req inc))
  ([req respond _raise]
   (respond (increment req))))


(defn decrement
  ([req]
   (update-signal req dec))
  ([req respond _raise]
   (respond (decrement req))))


(def counter-routes
  ["/counters/"
   ["" {:handler #'counters/counters}]
   ["increment/:id" #'increment]
   ["decrement/:id" #'decrement]])



;; -----------------------------------------------------------------------------
;; Form
;; -----------------------------------------------------------------------------
(def endpoint (form/->endpoint hk-gen/->sse-response))


(def form-routes
  ["/form"
   ["" {:handler #'form/form}]
   ["/endpoint" {:middleware [common/wrap-mpparams]
                 :handler #'endpoint}]])




(def router
  (rr/router
    [common/datastar-route
     counter-routes
     form-routes]))


(def handler
  (rr/ring-handler router
                   common/default-handler
                   {:middleware (into [[hk-gen/start-responding-middleware]] common/global-middleware)}))



