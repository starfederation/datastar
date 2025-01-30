(ns starfederation.datastar.clojure.protocols)


(defprotocol SSEGenerator
  (send-event! [this event-type data-lines opts] "Send sse event.")
  (close-sse! [this] "Close connection."))


