(ns starfederation.datastar.clojure.adapter.http-kit.impl
  (:require
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u]
    [org.httpkit.server :as hk-server])
  (:import
    [java.util.concurrent.locks ReentrantLock]
    [java.io ByteArrayOutputStream Closeable IOException]))


;; -----------------------------------------------------------------------------
;; Sending the headers
;; -----------------------------------------------------------------------------
(defn send-base-sse-response!
  "Send the response headers, this should be done as soon as the conneciton is
  open."
  [ch req {:keys [status] :as opts}]
  (hk-server/send! ch
                   {:status (or status 200)
                    :headers (ac/headers req opts)}
                   false))

;; -----------------------------------------------------------------------------
;; Sending events machinery
;; -----------------------------------------------------------------------------
(defn ->send-simple [ch opts]
  (let [buffer-size (or (ac/buffer-size opts)
                        ac/default-write-buffer-size)]
    (fn
      ([])
      ([event-type data-lines opts]
       (let [event (ac/build-event-str buffer-size event-type data-lines opts)]
         (hk-server/send! ch event false))))))


(defn flush-baos! [^ByteArrayOutputStream baos ch]
  (let [msg (.toByteArray baos)]
    (.reset baos)
    (hk-server/send! ch msg false)))


(defn ->send-gzip [ch opts]
  (let [^ByteArrayOutputStream baos (ByteArrayOutputStream.)
        {:keys [writer write!]} (ac/->write-machinery baos opts)]
    (fn
      ([]
       ;; Close the writer first to finish the gzip process
       (.close ^Closeable writer)
       ;; Flush towards SSE out
       (flush-baos! baos ch))
      ([event-type data-lines opts]
       (write! writer event-type data-lines opts)
       (ac/flush writer)
       (flush-baos! baos ch)))))


(defn ->send! [ch opts]
  (if (or (ac/gzip? opts)
          (ac/hold-write-buff? opts))
    (->send-gzip ch opts)
    (->send-simple ch opts)))

;; -----------------------------------------------------------------------------
;; SSE gen
;; -----------------------------------------------------------------------------
(deftype SSEGenerator [ch lock send!]
  p/SSEGenerator
  (send-event! [this event-type data-lines opts]
    (u/lock! lock
      (try
        (send! event-type data-lines opts)
        (catch IOException _
          (p/close-sse! this)
          false)
        (catch Exception e
          (throw (ex-info "Error sending SSE event"
                          {:sse-gen this
                           :event-type event-type
                           :data-lines data-lines
                           :opts opts}
                          e))))))

  (get-lock [_] lock)

  (close-sse! [_]
    (hk-server/close ch))

  (sse-gen? [_] true)

  Closeable
  (close [this]
    (p/close-sse! this)))


(defn ->sse-gen [ch send!]
  (SSEGenerator. ch (ReentrantLock.) send!))
