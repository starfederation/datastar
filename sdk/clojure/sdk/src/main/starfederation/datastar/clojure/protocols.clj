(ns starfederation.datastar.clojure.protocols)


(defprotocol SSEGenerator
  (send-event! [this event-type data-lines opts] "Send sse event.")
  (get-lock [this] "Access to the lock used in the generator.")
  (close-sse! [this] "Close connection.")
  (sse-gen? [this] "Test wheter a value is a SSEGenerator."))


(defn throw-not-implemented [type method]
  (throw (ex-info (str "Type " type " is not a SSEGenerator.") {:type type :method method})))


(extend-protocol SSEGenerator
  nil
  (sse-gen? [_] false)

  (send-event! [_this _event-type _data-lines _opts]
    (throw-not-implemented nil :send-event!))

  (get-lock [_this]
    (throw-not-implemented nil :get-lock))

  (close-sse! [_this]
    (throw-not-implemented nil :close-sse!))


  Object
  (sse-gen? [_] false)

  (send-event! [_this _event-type _data-lines _opts]
    (throw-not-implemented Object :send-event!))

  (get-lock [_this]
    (throw-not-implemented Object :get-lock))

  (close-sse! [_this]
    (throw-not-implemented Object :close-sse!)))

