(ns starfederation.datastar.clojure.adapter.ring.impl-test
  (:require
    [lazytest.core :as lt :refer [defdescribe it expect]]
    [ring.core.protocols :as p]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.adapter.ring.impl :as impl]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.common-test :refer [read-bytes]])
  (:import
    [java.io ByteArrayOutputStream]))

;; -----------------------------------------------------------------------------
;; Testing the gnarly adapter setup by simulating it
;; -----------------------------------------------------------------------------
(defn ->lock [] (volatile! 0))
(defn lock!   [!l] (vswap! !l inc))
(defn unlock! [!l] (vswap! !l dec))


(defn throw-key [k] (throw (ex-info "" {:k k})))
(defn throw-already-used  [] (throw-key :already-used-error))
(defn throw-writter-error [] (throw-key :writer-ctr-error))
(defn throw-flush-errror  [] (throw-key :flush-error))
(defn throw-on-open       [] (throw-key :on-open-error))


(defn set-writer! [!ref behavior]
  (case behavior
    :ok (vreset! !ref :new-writer)
    (throw-writter-error)))

(defn flush-headers! [v]
  (case v
    :ok nil
    (throw-flush-errror)))

(defn on-open [v]
  (case v
    :ok nil
    (throw-on-open)))


(defn write-body-to-stream-simulation
  "Here we mimick the behavior of the initialisation of ring SSE responses
  We capture lock, internal state, errors and return value to check several
  properties."
  [writer set-writer-v flush-v on-open-v]
  (let [!l (->lock)
        !writer (volatile! :old-writer)
        !return (volatile! nil)]
 
    ;; The code actually mimicking
    (try
      (lock! !l)
      (when writer
        (unlock! !l)
        (throw-already-used))

      (let [!error (volatile! nil)]
        (try
          (set-writer! !writer set-writer-v)
          (flush-headers! flush-v)
          (vreset! !return :success)
          (catch Throwable t
            (vreset! !error t))
          (finally
            (unlock! !l)
            (if-let [e @!error]
              (throw e)
              (on-open on-open-v)))))
      (catch Throwable t
        (vreset! !return t)))

    {:lock @!l
     :writer @!writer
     :return (let [r  @!return]
               (or
                 (-> r ex-data :k)
                 r))}))


(defn make-all-cases []
  (for [writer     [nil :old-writer]
        set-writer [:ok :throw]
        flush      [:ok :throw]
        on-open    [:ok :throw]]
    [writer set-writer flush on-open]))


(defn expected [writer set-writer! flush on-open]
  (cond
    writer                 {:lock 0 :writer :old-writer :return :already-used-error}
    (= set-writer! :throw) {:lock 0 :writer :old-writer :return :writer-ctr-error}
    (= flush       :throw) {:lock 0 :writer :new-writer :return :flush-error}
    (= on-open     :throw) {:lock 0 :writer :new-writer :return :on-open-error}
    :else                  {:lock 0 :writer :new-writer :return :success}))


(defn run-test-case [test-case]
  {:res (apply write-body-to-stream-simulation test-case)
   :expected  (apply expected test-case)})


(defn case-coherent? [test-case]
  (let [res (run-test-case test-case)]
    (= (:res res) (:expected res))))


(defdescribe simulate-write-body-to-stream
  (it "manages locks and errors properly"
   (doseq [test-case (make-all-cases)]
     (expect (case-coherent? test-case) (str test-case)))))        


;; -----------------------------------------------------------------------------
;; Basic sending of a SSE event without any server
;; -----------------------------------------------------------------------------
(defn send-SSE-event [response]
  (let [baos (ByteArrayOutputStream.)]
    (with-open [sse-gen (impl/->sse-gen)
                baos baos]
      (p/write-body-to-stream sse-gen response baos)
      (d*/merge-fragment! sse-gen "msg" {}))

    (expect
      (= (read-bytes baos (::impl/opts response))
         "event: datastar-merge-fragments\ndata: fragments msg\n\n\n"))))


(defdescribe simple-test
  (it "can send events with a using temp buffers"
    (send-SSE-event {}))
 
  (it "can send events with a using a persistent buffered reader"
    (send-SSE-event {::impl/opts {ac/write-profile ac/buffered-writer-profile}}))
  
  (it "can send gziped events with a using temp buffers"
    (send-SSE-event {::impl/opts {ac/write-profile ac/gzip-profile
                                  :gzip? true}}))
 
  (it "can send gziped events with a using a persistent buffered reader"
    (send-SSE-event {::impl/opts {ac/write-profile ac/gzip-buffered-writer-profile
                                  :gzip? true}})))
 



(comment
  (require '[lazytest.repl :as ltr])
  (ltr/run-test-var #'simulate-write-body-to-stream)
  (ltr/run-test-var #'simple-test))

