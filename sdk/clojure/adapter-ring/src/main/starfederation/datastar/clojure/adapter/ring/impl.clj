(ns starfederation.datastar.clojure.adapter.ring.impl
  (:require
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u]
    [ring.core.protocols :as rp])
  (:import
    [java.io  Closeable IOException OutputStream]
    java.util.concurrent.locks.ReentrantLock))


(defn ->send [os opts]
  (let [{:keys [writer write!]} (ac/->write-machinery os opts)]
    (fn
      ([]
       (.close ^Closeable writer))
     ([event-type data-lines event-opts]
      (write! writer event-type data-lines event-opts)
      (ac/flush writer)))))


;; Note that the send! field has 2 usages:
;; - it stores the sending function
;; - it acts as a `is-open?` flag
;; Also the on-close not being nil means the callback hasn't been called yet.
(deftype SSEGenerator [^:unsynchronized-mutable send!
                       ^ReentrantLock lock
                       ^:unsynchronized-mutable on-close]
  rp/StreamableResponseBody
  (write-body-to-stream [this request output-stream]
    (.lock lock)

    ;; already initialized, unlock and throw, we are out
    (when send!
      (.unlock lock)
      (throw (ex-info "Reused SSE-gen as several ring responses body. Don't do this." {})))

    (let [!error (volatile! nil)]
      (try
        ;; initializing the writing machinery
        (set! send! (->send output-stream (::opts request)))

        ;; flushing the HTTP headers
        (.flush ^OutputStream output-stream)
        true ;; dummy return
        ;; We catch everything here, if not a Throwable may pass through
        ;; !error won't catch it, on-open would be called
        (catch Throwable t
          (vreset! !error t))
        (finally
          ;; Any exception should have been caught,
          ;; the setup for the writing machinery is done,
          ;; the HTTP headers are sent
          ;; we can now release the lock now
          (.unlock lock)
          (if-let [e @!error]
            (throw e) ;; if error throw, the lock is already released
            ;; if all is ok call on-open, it can safely throw...
            (when-let [on-open (-> request ::opts :on-open)]
              (on-open this)))))))
 
  p/SSEGenerator
  (send-event! [this event-type data-lines opts]
    (u/lock! lock
      (if send! ;; still open?
        (try
          (send! event-type data-lines opts)
          true ;; successful send
          (catch IOException _
            ;; IOException means the connection is broken/closed,
            ;; we remove send! to let go of the writing machinery.
            ;; Also that way we don't try to close it again which would throw.
            (set! send! nil)
            ;; This exception is silenced, we make sure to call the closing fn
            ;; that will call the on-close callback
            (p/close-sse! this)
            false) ;; the event wasn't sent
          (catch Exception e
            ;; other exceptions should go through
            (throw (ex-info "Error sending SSE event"
                            {:sse-gen this
                             :event-type event-type
                             :data-lines data-lines
                             :opts opts}
                            e))))
        false))) ; closed return false

  (get-lock [_] lock)

  (close-sse! [this]
    (u/lock! lock
      ;; If either send! or on-close are here we try to close them
      (if (or send! on-close)
        (let [res (ac/close-sse! #(when send! (send!))
                                 #(when on-close (on-close this)))]
          ;; We make sure to clean them up after closing
          (set! send! nil)
          (set! on-close nil)
          (if (instance? Exception res)
            (throw res)
            true))
        false)))

  (sse-gen? [_] true)

  Closeable
  (close [this]
    (p/close-sse! this)))


(defn ->sse-gen [on-close]
  (SSEGenerator. nil
                 (ReentrantLock.)
                 on-close))

