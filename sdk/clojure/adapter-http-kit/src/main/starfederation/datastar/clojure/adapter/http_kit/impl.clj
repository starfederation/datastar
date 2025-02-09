(ns starfederation.datastar.clojure.adapter.http-kit.impl
  (:require
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u]
    [org.httpkit.server :as hk-server])
  (:import
    [java.util.concurrent.locks ReentrantLock]
    [java.io Closeable]))


;; -----------------------------------------------------------------------------
;; HTTP-KIT sse gen
;; -----------------------------------------------------------------------------
(defrecord HK-gen [ch lock]
  p/SSEGenerator
  (send-event! [_ event-type data-lines opts]
    (let [event-str (-> (StringBuilder.)
                        (sse/write-event! event-type data-lines opts)
                        str)]
      (u/lock! lock
        (hk-server/send! ch event-str false))))

  (close-sse! [_]
    (u/lock! lock
      (hk-server/close ch)))

  Closeable
  (close [this]
    (p/close-sse! this)))


(defn ->sse-gen [ch]
  (HK-gen. ch (ReentrantLock.)))


(defn send-base-sse-response! [ch req headers]
  (let [sse-headers (sse/headers req)]
    (hk-server/send! ch
                     {:status 200
                      :headers (merge headers sse-headers)}
                     false)))


