# Datastar Clojure SDK

We provide several libraries for working with [Datastar](https://data-star.dev/):

- A generic SDK to generate and send Datastar event using a SSE generator
  abstraction defined by the `starfederation.datastar.clojure.protocols/SSEGenerator`
  protocol. This gives us a common API working for each implementation of the protocol.
- Libraries containing implementations of the `SSEGenerator` protocol that work
  with specific ring adapters.
- A library containing [malli schemas](https://github.com/metosin/malli) for the SDK.

There currently are adapter implementations for:

- [ring](https://github.com/ring-clojure/ring)
- [http-kit](https://github.com/http-kit/http-kit)

If you want to roll your own adapter implementation, see
[implementing-adapters](/sdk/clojure/doc/implementing-adapters.md).

## Installation

For now the libraries are distributed as git dependencies. You need to add a dependency
for each library you use.

> [!important]
> This project is new and there isn't a release process yet other than using git shas.
> Replace `LATEST_SHA` in the git coordinates below by the actual latest commit sha of this repository.

To your `deps.edn` file you can add the following coordinates:

- SDK

```clojure
datastar/sdk {:git/url "https://github.com/starfederation/datastar/"
              :git/sha "LATEST_SHA"
              :deps/root "sdk/clojure/sdk"}
```

- ring implementation

```clojure
datastar/ring {:git/url "https://github.com/starfederation/datastar/"
               :git/sha "LATEST_SHA"
               :deps/root "sdk/clojure/adapter-ring"}}

ring-compliant/adapter "Coordinate for the ring compliant adater you wanna use."
```

> [!important]
> This is library that should work for ring compliant adapters,
> specifically those that use the `ring.core.protocols/StreamableResponseBody`
> protocol for their response bodies.

- http-kit implementation

```clojure
datastar/http-kit {:git/url "https://github.com/starfederation/datastar/"
                   :git/sha "LATEST_SHA"
                   :deps/root "sdk/clojure/adapter-http-kit"}}
```

- Malli schemas:

```clojure
datastar/malli-schemas {:git/url "https://github.com/starfederation/datastar/"
                        :git/sha "LATEST_SHA"
                        :deps/root "sdk/clojure/malli-schemas"}}
```

## Usage

### Concepts

By convention adapters provide a single `->sse-response` function. This
function returns a valid ring response tailored to work with the used ring
adapter. It takes callbacks that receive an implementation of the
SSE generator as the first argument.

You then use the Datastar SDK functions with the SSE generator.

### Short example

Start by requiring the api and an adapter. With HTTP-Kit for instance:

```clojure
(require '[starfederation.datastar.clojure.api :as d*])
         '[starfederation.datastar.clojure.adapter.http-kit :as hk-gen])

```

Using the adapter you create ring responses for your handlers:

```clojure
(defn sse-handler [request]
  (hk-gen/->sse-response request
    {:on-open
     (fn [sse-gen]
       (d*/merge-fragment! sse-gen "<div>test</div>")
       (d*/close-sse! sse-gen))}))

```

In the callback we use the SSE generator `sse-gen` with the Datastar SDK functions.

Depending on the adapter you use, you can keep the SSE generator open, store it
somewhere and use it later:

```clojure
(def !connections (atom #{}))


(defn sse-handler [request]
  (hk-gen/->sse-response request
    {:on-open
     (fn [sse-gen]
       (swap! !connections conj sse-gen))
     :on-close
     (fn [sse-gen]
       (swap! !connections disj sse-gen))}))


(defn broadcast-fragment! [fragment]
  (doseq [c @!connections]
    (d*/merge-fragment! c fragment)))

```

> [!important]
> Check doctrings / Readmes for the specific adapter you use.

> [!note]
> Check the docstrings in the `starfederation.datastar.clojure.api` namespace for
> more details on the SDK functions.

## Adapter differences:

Ring adapters are not made equals. Here are some the differences for the ones we provide:

| Adapter  | return values from the SDK event sending functions |
| -------- | -------------------------------------------------- |
| ring     | irrelevant                                         |
| http-kit | boolean, from `org.http-kit.server/send!`          |

> [!note]
> The SDK's event sending functions return whatever the adapter's implementation of
> `starfederation.datastar.clojure.protocols/send-event!` returns.

| Adapter  | connection lifetime in ring sync mode                                  |
| -------- | ---------------------------------------------------------------------- |
| ring     | same as the thread creating the sse response                           |
| http-kit | alive until the client or the server explicitely closes the connection |

> [!note]
> You may keep the connection open in ring-jetty sync mode by somehow blocking the thread
> handling the request.

| Adapter  | connection lifetime in ring async mode                                 |
| -------- | ---------------------------------------------------------------------- |
| ring     | alive until the client or the server explicitely closes the connection |
| http-kit | alive until the client or the server explicitely closes the connection |

| Adapter  | sending an event on closed connection              |
| -------- | -------------------------------------------------- |
| ring     | exception thrown                                   |
| http-kit | fn returns false, from `org.http-kit.server/send!` |

> [!note]
> This is one way to detect the connection has been closed by the client.

> [!important]
> At the moment, the ring-jetty adapter needs to send 2 small messages or 1 big
> message to detect a closed connection. There must be some buffering happening
> independent of our implementation.

| Adapter    | `:on-open` callback parameters |
| ---------- | ------------------------------ |
| ring-jetty | `[sse-gen]`                    |
| http-kit   | `[sse-gen]`                    |

| Adapter    | `:on-close` callback parameters |
| ---------- | ------------------------------- |
| ring-jetty | `[sse-gen]`                     |
| http-kit   | `[sse-gen status-code]`         |

## TODO:

- Streamlined release process (cutting releases and publish jar to a maven repo)
- Consider uniformizing the adapters behavior on connection closing (throwing in all adapters?)
- Review the etoin tests, there are some race conditions
- More official examples
