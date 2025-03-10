(ns starfederation.datastar.clojure.adapter.http-kit.impl-test
  (:require
    [lazytest.core :as lt :refer [defdescribe it expect]]
    [org.httpkit.server :as hk-server]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.adapter.http-kit.impl :as impl]
    [starfederation.datastar.clojure.adapter.common-test :refer [read-bytes]])
  (:import
    [java.io Closeable ByteArrayOutputStream OutputStreamWriter]))


(defrecord Channel [^ByteArrayOutputStream baos
                    !ch-open?
                    !on-close]
  hk-server/Channel
  ;; websocket stuff
  (open? [_] @!ch-open?)
  (websocket? [_] false)

  (on-receive [_ _callback])
  (on-ping [_ _callback])

  (close [_]
    (if @!ch-open?
      (do
        (vreset! !ch-open? false)
        (when-let [on-close @!on-close]
          (on-close :whatever))
        true)
      false))

  (on-close [_ callback]
    (vreset! !on-close callback))

  (send! [this data]
    (hk-server/send! this data true))

  (send! [this data close-after-send?]
    (cond
      (string? data)
      (let [^OutputStreamWriter osw (ac/->os-writer baos)]
        (doto osw
          (.append (str data))
          (.flush)))

      (bytes? data)
      (-> baos
          (.write (bytes data))))

    (when close-after-send?
      (hk-server/close this))))


(defn ->channel [baos]
  (Channel. baos
           (volatile! true)
           (volatile! nil)))


(defn ->sse-gen [baos opts]
  (let [c (->channel baos)
        send! (impl/->send! c opts)]
    (hk-server/on-close c
      (fn [status]
        (send!)
        (when-let [callback (:on-close opts)]
          (callback c status))))
    (impl/->sse-gen c send!)))


(defn send-SSE-event [opts]
  (let [baos (ByteArrayOutputStream.)]
    (with-open [_baos baos
                sse-gen ^Closeable (->sse-gen baos opts)]
      (d*/merge-fragment! sse-gen "msg" {}))

    (expect
      (= (read-bytes baos opts)
         "event: datastar-merge-fragments\ndata: fragments msg\n\n\n"))))


(defdescribe simple-test
  (it "We can send events using a temp buffer"
    (send-SSE-event {}))
 
  (it "We can send events using a persistent buffered reader"
    (send-SSE-event {ac/write-profile ac/buffered-writer-profile}))
   
  (it "We can send gziped events using a temp buffer"
    (send-SSE-event {ac/write-profile ac/gzip-profile :gzip? true}))
 
  (it "We can send gziped events using a persistent buffered reader"
    (send-SSE-event {ac/write-profile ac/gzip-buffered-writer-profile :gzip? true})))
 

(comment
  (require '[lazytest.repl :as ltr])
  (ltr/run-test-var #'simple-test)
  :dbg
  :rec)
 

