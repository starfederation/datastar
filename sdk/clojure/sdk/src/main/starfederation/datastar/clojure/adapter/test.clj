(ns starfederation.datastar.clojure.adapter.test
  (:require
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u])
 (:import
    [java.util.concurrent.locks ReentrantLock]
    [java.io Closeable]))



(deftype ReturnMsgGen []
  p/SSEGenerator
  (send-event! [_ event-type data-lines opts]
    (-> (StringBuilder.)
        (sse/write-event! event-type data-lines opts)
        str))

  (close-sse! [_]))



(defn ->sse-gen [& _]
  (->ReturnMsgGen))




(deftype RecordMsgGen [lock !rec !open?]
  p/SSEGenerator
  (send-event! [_ event-type data-lines opts]
    (u/lock! lock
      (vswap! !rec conj (-> (StringBuilder.)
                            (sse/write-event! event-type data-lines opts)
                            str))))

  (close-sse! [_]
    (u/lock! lock
      (vreset! !open? false)))

  Closeable
  (close [this]
    (p/close-sse! this)))

(java.util.ArrayList. 1)

(defn ->sse-response
  "Fake a sse-response, the events sent with sse-gen during the
  `on-open` callback are recorded in a vector stored in an atom returned as the
  body of the response."
  [req {:keys [status headers on-open]}]
  (let [!rec (volatile! [])
        sse-gen (->RecordMsgGen (ReentrantLock.)
                                !rec
                                (volatile! true))]
    (on-open sse-gen)
    {:status (or status 200)
     :headers (merge headers (sse/headers req))
     :body !rec}))


