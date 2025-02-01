(ns starfederation.datastar.clojure.adapter.test
  (:require
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.protocols :as p]))


(deftype ReturnMsgGen []
  p/SSEGenerator
  (send-event! [_ event-type data-lines opts]
    (-> (StringBuilder.)
        (sse/write-event! event-type data-lines opts)
        str))

  (close-sse! [_]))



(defn ->sse-gen [& _]
  (->ReturnMsgGen))

