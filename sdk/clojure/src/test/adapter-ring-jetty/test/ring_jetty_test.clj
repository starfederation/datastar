(ns test.ring-jetty-test
  (:require
    [test.common :as common]
    [test.examples.ring-jetty-handler :as rjh]
    [lazytest.core :as lt :refer [defdescribe expect it]]
    [ring.adapter.jetty :as jetty]
    [starfederation.datastar.clojure.adapter.ring-jetty :as jetty-gen])
  (:import
    [org.eclipse.jetty.server Server ServerConnector]))

;; -----------------------------------------------------------------------------
;; Ring Jetty stuff
;; -----------------------------------------------------------------------------
(defn stop-jetty! [^Server server]
  (.stop server))


(defn jetty-server-port [jetty-server]
  (let [connector (-> jetty-server
                      Server/.getConnectors
                      seq
                      first)]
    (.getLocalPort ^ServerConnector connector)))


(def ring-jetty-basic-opts
  {:start! jetty/run-jetty
   :stop!  stop-jetty!
   :get-port jetty-server-port
   :join? false})


;; -----------------------------------------------------------------------------
;; Tests
;; -----------------------------------------------------------------------------
(defdescribe  counters-test
  {:webdriver true
   :context [(common/with-server-f rjh/handler ring-jetty-basic-opts)]}
  (it "manages signals"
    (doseq [[driver-type driver] common/drivers]
      (let [res (common/run-counters! @driver)]
        (expect (= res common/expected-counters) (str driver-type))))))


(defdescribe counters-async-test
  {:webdriver true
   :context [(common/with-server-f rjh/handler
                                   (assoc ring-jetty-basic-opts :async? true))]}
  (it "manages signals"
    (doseq [[driver-type driver] common/drivers]
      (let [res (common/run-counters! @driver)]
        (expect (= res common/expected-counters) (str driver-type))))))

;; -----------------------------------------------------------------------------
(defdescribe form-test
  {:webdriver true
   :context [(common/with-server-f rjh/handler ring-jetty-basic-opts)]}
  (it "manages forms"
    (doseq [[driver-type driver] common/drivers]
      (let [res (common/run-form-test! @driver)]
        (expect (= res common/expected-form-vals) (str driver-type))))))


(defdescribe form-test-async
  {:webdriver true
   :context [(common/with-server-f rjh/handler
                                   (assoc ring-jetty-basic-opts :async? true))]}
  (it "manages forms"
    (doseq [[driver-type driver] common/drivers]
      (let [res (common/run-form-test! @driver)]
        (expect (= res common/expected-form-vals) (str driver-type))))))


;; -----------------------------------------------------------------------------
(defdescribe persistent-sse-test
  "Testing persistent connection, events are sent from outide the ring handler."
  {:context [(common/persistent-sse-f jetty-gen/->sse-response
                                      (assoc ring-jetty-basic-opts
                                             :async? true))]}
  (it "handles persistent connections"
    (let [res (common/run-persistent-sse-test!)]
      (expect (map? res))
      (common/p-sse-status-ok? res)
      (common/p-sse-http1-headers-ok? res)
      (common/p-sse-body-ok? res))))




