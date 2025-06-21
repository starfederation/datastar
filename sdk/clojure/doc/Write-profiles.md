# Write profiles

To manage several aspects of a SSE connection
(see the [SSE design notes](./SSE-design-notes.md)) the SDK provides a `write profile`
mechanism. It lets you control:

- the buffering behavior of the SSE connection
- whether you want to add compression to the SSE stream

## Example

An example may be the quickest way to get started.

Let's say we want to have a ring SSE handler using gzip compression with a
temporary write buffer strategy. We can create a write profile to do this.

```clojure
(require
  '[starfederation.datastar.clojure.adapter.common :as ac])

(def my-write-profile
   ;; We specify a function that will wrap the output stream
   ;; used for the SSE connection
  {ac/wrap-output-stream (fn [os] (-> os ac/->gzip-os ac/->os-writer))

   ;; We specify which writing function to use on the output stream
   ;; Since we just use an OutputStreamWriter in the wrap function above
   ;; we go for the temp buffer writing function helper
   ac/write! (ac/->write-with-temp-buffer!)

   ;; We also provide a content encoding header for the HTTP response
   ;; this way it is automatically added
   ac/content-encoding ac/gzip-content-encoding})

```

When using the `->sse-response` function we can do:

```clojure
(require
  '[starfederation.datastar.clojure.api :as d*]
  '[starfederation.datastar.clojure.adapter.ring :refer [->sse-response on-open]])

(defn handler [req]
  (->sse-response req
    {ac/write-profile my-write-profile ;; note the use of the write profile here
     on-open
     (fn [sse]
       (d*/with-open-sse sse
         (d*/patch-elements! sse "some big element")))}))
```

This response will have the right `Content-Encoding` header and will compress
SSE event with gzip.

If we want to control the buffer sizes used by our output stream we can
write another profile:

```clojure

(def my-specific-write-profile
  {ac/wrap-output-stream
   (fn [os] (-> os
                (ac/->gzip-os 1024) ;; setting the gzip os buffer size
                ac/->os-writer))

   ac/write! (ac/->write-with-temp-buffer! 16384);; initial size of the StringBuilder
   ac/content-encoding ac/gzip-content-encoding})

```

This also allows for using other compression algorithms as long as they work
like java's `java.util.zip.GZIPOutputStream`. We could also implement a buffer
pooling of some kind by providing a custom `ac/write!` function.

## SDK provided write profiles

The SDK tries to provide sensible defaults. There are several write profiles
provided:

| profile                      | compression | buffering strategy       | write! helper               |
| ---------------------------- | ----------- | ------------------------ | --------------------------- |
| basic-profile                | no          | temporary StringBuilder  | `->write-with-temp-buffer!` |
| buffered-writer-profile      | no          | permanent BufferedWriter | `write-to-buffered-writer!` |
| gzip-profile                 | gzip        | temporary StringBuilder  | `->write-with-temp-buffer!` |
| gzip-buffered-writer-profile | gzip        | permanent BufferedWriter | `write-to-buffered-writer!` |

If you don't specify a profile in the `->sse-response` function call, the basic
profile is used by default.

With `->write-with-temp-buffer!`, the underlying `StringBuilder` default size
is modeled after java's `BufferedWriter`, that is 8192 bytes.

The rest of the buffer sizes are java's defaults.

Note that we have specific helper used for the `ac/write!` value, depending on
the buffering strategy.

Each adapter specific namespace aliases the write profile option key and
the profiles provided by the SDK.

## Adapter specific behavior

Http-kit doesn't use an `OutputStream` as its IO primitive. This means that as
soon as you want to wrap an `OutputStream` the Http-kit adapter will create and
hold onto a `ByteArrayOutputStream` for your `ac/wrap-output-stream` function
to wrap. It will take the bytes from this `OutputStream` to send.

This has an impact on allocated memory and so this behavior is used for the
buffered-writer-profile, the gzip-profile and the gzip-buffered-writer-profile.

The basic-profile just concatenates SEE events text in a String builder and sends
the text. It doesn't allocate any `ByteArrayOutputStream`.
