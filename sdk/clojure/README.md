# Datastar Clojure SDK

We provide several libraries for working with [Datastar](https://data-star.dev/):

- A generic SDK to generate and send SSE events formatted for Datastar.
- A SSE generator abstraction defined by the
  `starfederation.datastar.clojure.protocols/SSEGenerator` protocol as well as
  several libraries implementing it to work with specific ring adapters.
- A library containing [malli schemas](https://github.com/metosin/malli)
  covering the generic API and our adapter implementations.
- A library providing the tools necessary to use Brotli compression in SSE Streams

There currently are adapter implementations for:

- [ring](https://github.com/ring-clojure/ring)
- [http-kit](https://github.com/http-kit/http-kit)

If you want to roll your own adapter implementation, see
[implementing-adapters](/sdk/clojure/doc/implementing-adapters.md).

## Installation

To your `deps.edn` file you can add the following coordinates:

| library       | deps coordinate                                                                                                              |
| ------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| SDK           | [![](https://clojars.org/dev.data-star.clojure/latest-version.svg)](https://clojars.org/dev.data-star.clojure/sdk)           |
| http-kit      | [![](https://clojars.org/dev.data-star.clojure/latest-version.svg)](https://clojars.org/dev.data-star.clojure/http-kit)      |
| ring          | [![](https://clojars.org/dev.data-star.clojure/latest-version.svg)](https://clojars.org/dev.data-star.clojure/ring)          |
| brotli        | [![](https://clojars.org/dev.data-star.clojure/latest-version.svg)](https://clojars.org/dev.data-star.clojure/brotli)        |
| malli-schemas | [![](https://clojars.org/dev.data-star.clojure/latest-version.svg)](https://clojars.org/dev.data-star.clojure/malli-schemas) |

Notes:

- You need the sdk and either the http-kit or the ring library to get started.
- The ring library works with ring compliant adapters (adapter using the
  `ring.core.protocols/StreamableResponseBody`)
- Currently the brotli library works only with http-kit

## Usage

### Basic Concepts

By convention SDK adapters provide a single `->sse-response` function. This
function returns a valid ring response tailored to work with the ring
adapter it is made for. This function will receive an implementation of
`SSEGenerator` protocol also tailored to the ring adapter used.

You then use the Datastar SDK functions with the SSE generator.

### Short example

Start by requiring the main API and an adapter. With Http-kit for instance:

```clojure
(require '[starfederation.datastar.clojure.api :as d*])
         '[starfederation.datastar.clojure.adapter.http-kit :as hk-gen])

```

Using the adapter you create ring responses in your handlers:

```clojure
(defn sse-handler [request]
  (hk-gen/->sse-response request
    {hk-gen/on-open
     (fn [sse-gen]
       (d*/patch-elements! sse-gen "<div>test</div>")
       (d*/close-sse! sse-gen))}))

```

In the callback we use the SSE generator `sse-gen` with the Datastar SDK functions.

Depending on the adapter you use, you can keep the SSE generator open by storing
it somewhere and use it later:

```clojure
(def !connections (atom #{}))


(defn sse-handler [request]
  (hk-gen/->sse-response request
    {hk-gen/on-open
     (fn [sse-gen]
       (swap! !connections conj sse-gen))

     hk-gen/on-close
     (fn [sse-gen status]
       (swap! !connections disj sse-gen))}))


(defn broadcast-elements! [elements]
  (doseq [c @!connections]
    (d*/patch-elements! c elements)))

```

Check the docstrings in the `starfederation.datastar.clojure.api` namespace for
more details.

### Advanced features

This SDK is essentially a tool to manage SSE connections with helpers to format
events the way the Datastar framework expects them on the front end.

It provides advanced functionality for managing several aspects of SSE.

You can find more information in several places:

- the docstings for the `->sse-response` function you are using.
- the [SSE design notes document](/sdk/clojure/doc/SSE-design-notes.md) details
  what considerations are taken into account in the SDK.
- the [write profiles document](/sdk/clojure/doc/Write-profiles.md) details the
  tools the SDK provides to control the buffering behaviors of a SSE stream and
  how to use compression.
- the [adapter implementation guide](/sdk/clojure/doc/implementing-adapters.md)
  lists the conventions by which SDK adapters are implemented if the need to
  implement your own ever arises.

## Adapter behaviors:

Ring adapters are not made equals. This leads to our SSE generators not having
the exact same behaviors in some cases.

### SSE connection lifetime in ring when trying to store the sse-gen somewhere

#### With ring sync

| Adapter  |                                                                        |
| -------- | ---------------------------------------------------------------------- |
| ring     | same as the thread creating the sse response                           |
| http-kit | alive until the client or the server explicitely closes the connection |

You may keep the connection open in ring sync mode by somehow blocking the thread
handling the request. This is a valid strategy when using java's virtual threads.

#### With ring async

| Adapter      |                                                                        |
| ------------ | ---------------------------------------------------------------------- |
| all adapters | alive until the client or the server explicitely closes the connection |

### Detecting a closed connection

| Adapter  |                                                                                                 |
| -------- | ----------------------------------------------------------------------------------------------- |
| ring     | Sending events on a closed connection will fail at some point and the sse-gen will close itself |
| http-kit | Http-kit detects closed connections by itself and closes the sse-gen                            |

At this moment, when using the ring adapter and jetty, our SSE generators need
to send 2 small events or 1 big event to detect a closed connection.
There must be some buffering happening independent of our implementation.

## TODO:

- Streamlined release process (cutting releases and publish jar to a maven repo)
- Review the etaoin tests, there are some race conditions

## License

[![License](https://img.shields.io/github/license/starfederation/datastar)](https://github.com/starfederation/datastar/blob/main/LICENSE)
