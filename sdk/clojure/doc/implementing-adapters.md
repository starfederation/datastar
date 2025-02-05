# Implementing adapters

If you are using a ring adapter not supported by this library or if you want to
roll your own there are helpers to facilitate making one. At minimum you need to
implement 1 protocol. If you wanna be more in line with the provided adapters
there are more conventions to follow.

Also, for the library as a whole we try to stay close to the
[SDK's design document](/sdk/README.md) used for all SDKs.

## Implementing the `SSEGenerator` protocol

An SSE generator is made by implementing the
`starfederation.datastar.clojure.protocols/SSEGenerator` protocol.

There are 2 functions to implement:

- `(send-event! [this event-type data-lines opts] )`
  This function must contain the logic to actually send a SSE event.
- `(close-sse! [this] "Close connection.")`
  This function must close the connection use by the `SSEGenerator`.

### Implementing `send-event!`

To help implement this function you should use the
`starfederation.datastar.clojure.api.sse/write-event!` function.

It take 4 arguments:

- `buffer`: A `java.lang.Appendable`
- `event-type`: a string representing a Datastar event type
- `data-lines`: a seq of data lines constituting the 'body' of the event
- `opts`: a map of SSE Options.

You actually don't need to care about anything other than the `buffer` with this function,
the generic SDK will provide the value for the other arguments.

For instance implementing a test generator that return the event's text instead
of sending it looks like:

```clojure
(deftype ReturnMsgGen []
  p/SSEGenerator
  (send-event! [_ event-type data-lines opts]
    (-> (StringBuilder.)
        (sse/write-event! event-type data-lines opts) ; just pass the parameters down
        str)) ; we return the event string instead of sending it

  (close-sse! [_]))


(defn ->sse-gen [& _]
  (->ReturnMsgGen))

```

As per the design doc that all Datastar SDKs follow, we use a lock in this
function (`java.util.concurrent.locks.ReentrantLock`) to protect
from several threads concurrently writing any underlying buffer before flushing.

See `starfederation.datastar.clojure.utils/lock!` for a helper with the
Reentrant locks.

> [!note]
> The lock is not needed in this example, since the buffer is created for each call.
> However it is necessary when the buffer is shared.

### Implementing `close-sse!`

Just close whatever constitutes a connection for your particular adapter.

If you follow the conventions detailed below, you must call an `on-close`
callback in this function.

## Conventions followed in the provided adapters

The provided adapters follow some conventions beyond the `SSEGenerator` protocol.
You can take a look at how they are implemented and replicate the API.

### The `->sse-response` function

Provided adapters have a single `->sse-response` function for an API.

This function takes 2 arguments:

- the ring request
- a map whose keys are:
  - `:status`: the HTTP status for the response
  - `:headers`: a map of `str -> str`, HTTP headers to add to the response.
  - `:on-open`: a mandatory callback that must be called when the SSE connection is opened.
    It has 1 argument, the SSE Generator.
  - `:on-close`: A callback called when the SSE connection is closed.
    Each adapter may have a different parameters list for this callback, depending on what
    is relevant. Still the first parameter should be the SSE generator.

It has 2 responsibilities:

- This function creates the SSE generator, gives the callbacks to it.
- It must create a valid ring response with the correct HTTP SSE headers and
  merge the headers provided with the `:headers` option.
  See `starfederation.datastar.clojure.api.sse/headers`.

### `SSEGenerator` additional logic

The implementation must call the `on-open` callback when the underlying connection is opened.

### The `close-sse!` function

This function must call the `on-close` callback provided when using the `->sse-response`
function. Here the lock used when writing an event is reused when closing the underlying
connection.
