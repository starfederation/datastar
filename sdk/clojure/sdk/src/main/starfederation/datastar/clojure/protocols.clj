(ns starfederation.datastar.clojure.protocols)


(defprotocol SSEGenerator
  (send-event! [this event-type data-lines opts] "Send sse event.")
  (get-lock [this] "Access to the lock used in the generator.")
  (close-sse! [this] "Close connection.")
  (sse-gen? [this] "Test wheter a value is a SSEGenerator."))


(extend-protocol SSEGenerator
  nil
  (sse-gen? [_] false)

  Object
  (sse-gen? [_] false))


