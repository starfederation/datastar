(ns starfederation.datastar.clojure.adapter.ring.impl
  (:require
    [clojure.java.io :as io]
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u]
    [ring.core.protocols :as rp])
  (:import
    java.io.Closeable
    java.io.OutputStream
    java.io.BufferedWriter
    java.util.concurrent.locks.ReentrantLock))


(defrecord SSE-gen [writer lock on-open on-close]
  rp/StreamableResponseBody
  (write-body-to-stream [this _ output-stream]
    (u/lock! lock
      (when (deref writer)
        (throw (ex-info "Reused SSE-gen as several ring responses body. Don't do this." {})))
      (.flush ^OutputStream output-stream)
      (vreset! writer (io/writer output-stream)))
    (on-open this))


  p/SSEGenerator
  (send-event! [this event-type data-lines opts]
    (u/lock! lock
      (try
        (doto ^BufferedWriter @writer
          (sse/write-event! event-type data-lines opts)
          (.flush))
        (catch Exception e
          (throw (ex-info "Error sending SSE event"
                          {:sse-gen this
                           :event-type event-type
                           :data-lines data-lines
                           :opts opts}
                          e))))))

  (close-sse! [this]
    (u/lock! lock
      (when-let [^BufferedWriter w @writer]
        (.close w)
        (when on-close
          (on-close this)))))

  Closeable
  (close [this]
    (p/close-sse! this)))


(defn ->sse-gen [on-open on-close]
  (SSE-gen. (volatile! nil)
            (ReentrantLock.)
            on-open
            on-close))


