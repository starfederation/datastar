(ns test.http-kit-test
  (:require
    [test.common :as common]
    [test.examples.http-kit-handler :as hkh]
    [lazytest.core :as lt :refer [defdescribe expect it]]
    [org.httpkit.server :as hk-server]
    [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]))

;; -----------------------------------------------------------------------------
;; HTTP-Kit stuff
;; -----------------------------------------------------------------------------
(def http-kit-basic-opts
  {:start! hk-server/run-server
   :stop!  hk-server/server-stop!
   :get-port hk-server/server-port
   :legacy-return-value? false})


;; -----------------------------------------------------------------------------
(defdescribe counters-test
  {:webdriver true
   :context [(common/with-server-f hkh/handler http-kit-basic-opts)]}
  (it "manages signals"
    (doseq [[driver-type driver] common/drivers]
      (let [res (common/run-counters! @driver)]
        (expect (= res common/expected-counters) (str driver-type))))))


(defdescribe counters-test-async
  {:webdriver true
   :context [(common/with-server-f hkh/handler (assoc http-kit-basic-opts
                                                      :ring-async? true))]}
  (it "manages signals"
    (doseq [[driver-type driver] common/drivers]
      (let [res (common/run-counters! @driver)]
        (expect (= res common/expected-counters) (str driver-type))))))

;; -----------------------------------------------------------------------------
;; Tests
;; -----------------------------------------------------------------------------
(defdescribe form-test
  {:webdriver true
   :context [(common/with-server-f hkh/handler http-kit-basic-opts)]}
  (it "manages forms"
    (doseq [[driver-type driver] common/drivers]
      (let [res (common/run-form-test! @driver)]
        (expect (= res common/expected-form-vals) (str driver-type))))))


;; -----------------------------------------------------------------------------
(defdescribe persistent-sse-test
  {:context [(common/persistent-sse-f hk-gen/->sse-response
                                      http-kit-basic-opts)]}
  (it "handles persistent connections"
    (let [res (common/run-persistent-sse-test!)]
      (expect (map? res))
      (expect (common/p-sse-status-ok? res))
      (expect (common/p-sse-http1-headers-ok? res))
      (expect (common/p-sse-body-ok? res)))))


(defdescribe persistent-sse-test-async
  {:context [(common/persistent-sse-f hk-gen/->sse-response
                                      (assoc http-kit-basic-opts
                                             :ring-async? true))]}
  (it "handles persistent connections"
    (let [res (common/run-persistent-sse-test!)]
      (expect (map? res))
      (common/p-sse-status-ok? res)
      (common/p-sse-http1-headers-ok? res)
      (common/p-sse-body-ok? res))))

