(ns starfederation.datastar.clojure.adapter.http-kit.impl
  (:require
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u]
    [org.httpkit.server :as hk-server])
  (:import
    [java.util.concurrent.locks ReentrantLock]
    [java.io ByteArrayOutputStream Closeable]))


(def basic-profile
  "Basic write profile using temporary [[StringBuilder]]s, no output stream and
  no compression."
  {ac/write! (ac/->build-event-str)})


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
(defn ->send-simple [ch write-profile]
  (let [write! (ac/write! write-profile)]
    (fn
      ([])
      ([event-type data-lines opts]
       (let [event (write! event-type data-lines opts)]
         (hk-server/send! ch event false))))))


(defn flush-baos! [^ByteArrayOutputStream baos ch]
  (let [msg (.toByteArray baos)]
    (.reset baos)
    (hk-server/send! ch msg false)))


(defn ->send-with-output-stream [ch write-profile]
  (let [^ByteArrayOutputStream baos (ByteArrayOutputStream.)
        {wrap-os ac/wrap-output-stream
         write! ac/write!} write-profile
        writer (wrap-os baos)]
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
  (let [write-profile (or (ac/write-profile opts)
                          basic-profile)]
    (if (ac/wrap-output-stream write-profile)
      (->send-with-output-stream ch write-profile)
      (->send-simple ch write-profile))))

;; -----------------------------------------------------------------------------
;; SSE gen
;; -----------------------------------------------------------------------------
(deftype SSEGenerator [ch lock send! on-exception]
  p/SSEGenerator
  (send-event! [this event-type data-lines opts]
    (u/lock! lock
      (try
        (send! event-type data-lines opts)
        (catch Exception e
          (when (on-exception this e {:sse-gen this
                                      :event-type event-type
                                      :data-lines data-lines
                                      :opts opts})
            (p/close-sse! this))
          false))))

  (get-lock [_] lock)

  (close-sse! [_]
    (hk-server/close ch))

  (sse-gen? [_] true)

  Closeable
  (close [this]
    (p/close-sse! this)))


(defn ->sse-gen
  ([ch send!]
   (->sse-gen ch send! {}))
  ([ch send! opts]
   (SSEGenerator. ch (ReentrantLock.) send! (or (ac/on-exception opts)
                                                ac/default-on-exception))))
