(ns starfederation.datastar.clojure.adapter.common
  "
  Namespace containing the shared code for each adapter.

  It contains helpers for working with [[OutputStreams]] and assembling SSE
  sending function using several strategies and managing exceptions in the
  adapters.

  The main concept here is what we call \"write profiles\". A write profile is
  a map of 3 keys:
  - [[wrap-output-stream]]
  - [[write!]]
  - [[content-encoding]]

  With this 3 keys we can control buffering aspects of our SSE connction and
  and compression functionality.

  Here is an example profile which uses gzip and temporary buffers to
  concatenate SSE event text:

  ```clojure
  (def gzip-profile
    {wrap-output-stream (fn[^OutputStream os] (-> os ->gzip-os ->os-writer))
     content-encoding gzip-content-encoding
     write! (->write-with-temp-buffer!)})
  ```

  The `wrap-output-stream` function will use a [[GZIPOutputStream]] and an
  [[OutputStreamWriter]] constructed with the [[->gzip-os]] and [[->os-writer]]
  helpers.

  To go with this we use a `write!` function constructed with
  [[->write-with-temp-buffer!]].

  If we wanted specific buffer sizes we could do:

  ```clojure
  (def gzip-profile
    {wrap-output-stream (fn[^OutputStream os]
                          (-> os
                              (->gzip-os 1024)
                              ->os-writer))
     content-encoding gzip-content-encoding
     write! (->write-with-temp-buffer! 4096)})
  ```

  The output stream wrapping helpers are:
  - [[->gzip-os]]
  - [[->os-writer]]
  - [[->buffered-writer]]

  The write function helper to go with them are:
  - [[->write-with-temp-buffer!]]
  - [[write-to-buffered-writer!]]

  See the rest of the docstrings for more details.
"
  (:refer-clojure :exclude [flush])
  (:require
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u])
  (:import
    [java.io
     BufferedWriter Flushable IOException
     OutputStream OutputStreamWriter Writer]
    [java.nio.charset Charset StandardCharsets]
    java.util.zip.GZIPOutputStream))


;; -----------------------------------------------------------------------------
;; Advanced SSE options
;; -----------------------------------------------------------------------------
(def write-profile
  "SSE option key:

  This option allows a user to control the way SSE events are assembled and
  sent. It take a write profile which is a map whose keys are:
  - [[wrap-output-stream]]
  - [[write!]]
  - [[content-encoding]] (optional if you don't use compression)
  "
  :d*.sse/write-profile)


(def wrap-output-stream
  "SSE write profile key:

  A function that wraps an OutputStream like:
  `(fn [os] (clojure.java.io/writer os))`

  This function will be used to wrap the [[OutputStream]] used for a SSE
  connection.

  It allows you to add compression and buffering to suit you needs.

  The SDK provides utilities to implement such a wrapping function, see:
  - [[->gzip-os]]
  - [[->os-writer]]
  - [[->buffered-writer]]
  "
  :d*.sse.write-profile/wrap-output-stream)


(def write!
  "SSE write profile option:

  A function that writes to a [[java.lang.Appendable]]. It should go in tandem
  with the way you wrap the [[OutputStream]].

  The SDK provide pre-made write function, see:
  - [[->write-with-temp-buffer!]]
  - [[write-to-buffered-writer!]]
  "
  :d*.sse.write-profile/write!)


(def content-encoding
  "SSE write profile option:

  A string value for the Content-Encoding HTTP header. When using gzip
  compression the value should be [[gzip-content-encoding]].
  "
  :d*.sse.write-profile/content-encoding)


;; -----------------------------------------------------------------------------
;; HTTP headers helper
;; -----------------------------------------------------------------------------
(defn headers
  "Same as [[sse/headers]] with the added responsibility to add the
  Content-Encoding header based on a write profile."
  [ring-request & {:as opts}]
  (let [encoding (-> opts write-profile content-encoding)]
    (-> (transient {})
        (u/merge-transient! sse/base-SSE-headers)
        (cond->
          (sse/http1? ring-request) (assoc! "Connection"       "keep-alive",)
          encoding                  (assoc! "Content-Encoding" encoding))
        (u/merge-transient! (:headers opts))
        persistent!)))


(comment
  (headers {:protocol "HTTP/2"} {})
  (headers {:protocol "HTTP/2"} {write-profile {content-encoding "br"}}))

;; -----------------------------------------------------------------------------
;; Utilities for wrapping an OutputStream
;; -----------------------------------------------------------------------------
(defn ->gzip-os
  "Make a GZIPOutputStream from an OutputStream `os` and an optional
  `buffer-size`. The syncFlush always set to true.

  Default buffer-size is the GZIPOutputStream's own.
  "
  ([^OutputStream os]
   (GZIPOutputStream. os true))
  ([^OutputStream os buffer-size]
   (GZIPOutputStream. os buffer-size true)))


(defn ->os-writer
  "Make an OutputStreamWriter from an OutputStream `os` and an optional
  [[Charset]] `charset`.

  Defaut charset is [[StandardCharsets/UTF_8]]
  "
  ([^OutputStream os]
   (OutputStreamWriter. os StandardCharsets/UTF_8))
  ([^OutputStream os ^Charset charset]
   (OutputStreamWriter. os charset)))


(defn ->buffered-writer
  "Make an BufferedWriter from an OutputStreamWriter `osw` and an optional
  `buffer-size`.

  Default buffer-size is the BufferedWriter's own.
  "
  ([osw]
   (BufferedWriter. osw))
  ([osw buffer-size]
   (BufferedWriter. osw buffer-size)))


;; -----------------------------------------------------------------------------
;; Writing utilities
;; -----------------------------------------------------------------------------
(def default-write-buffer-size 8192)


(defn ->build-event-str
  "Make a function that will assemble a SSE event using a StringBuiler and
  return the concatenated string."
  ([]
   (->build-event-str default-write-buffer-size))
  ([buffer-size]
   (fn build-event-str [event-type data-lines opts]
     (-> (StringBuilder. (int buffer-size))
         (sse/write-event! event-type data-lines opts)
         str))))


(defn ->write-with-temp-buffer!
  "Make a function that will assemble a SSE event using a [[StringBuilder]] and
  then will write the resulting string to an [[OutputStreamWriter]]. "
  ([]
   (->write-with-temp-buffer! default-write-buffer-size))
  ([buffer-size]
   (let [build-event (->build-event-str buffer-size)]
     (fn write-with-temp-buffer! [^Writer writer event-type data-lines opts]
       (let [event-str (build-event event-type data-lines opts)]
         (.append writer ^String event-str))))))


(defn write-to-buffered-writer!
  "Write a SSE event to a stream using a [[BufferedWriter]]."
  [^BufferedWriter writer event-type data-lines opts]
  (sse/write-event! writer event-type data-lines opts))


(defn flush
  "Flush a `java.io.Flushable`."
  [^Flushable f]
  (.flush f))

;; -----------------------------------------------------------------------------
;; SDK provided write profiles
;; -----------------------------------------------------------------------------
(def gzip-content-encoding "gzip")

(def basic-profile
  "Basic write profile using temporary [[StringBuilder]]s and no compression."
  {wrap-output-stream (fn[^OutputStream os] (-> os ->os-writer))
   write! (->write-with-temp-buffer!)})

(def buffered-writer-profile
  "Write profile using a permanent [[BufferedWriter]] and no compression."
  {wrap-output-stream (fn[^OutputStream os] (-> os ->os-writer ->buffered-writer))
   write! (->write-with-temp-buffer!)})

(def gzip-profile
  "Write profile using temporary [[StringBuilder]]s and gzip compression."
  {wrap-output-stream (fn[^OutputStream os] (-> os ->gzip-os ->os-writer))
   content-encoding gzip-content-encoding
   write! (->write-with-temp-buffer!)})

(def gzip-buffered-writer-profile
  "Write profile using a permanent [[BufferedWriter]] and gzip compression."
  {wrap-output-stream (fn[^OutputStream os] (-> os ->gzip-os ->os-writer ->buffered-writer))
   content-encoding gzip-content-encoding
   write! write-to-buffered-writer!})


;; -----------------------------------------------------------------------------
;; Exception handling / Closing sse helpers
;; -----------------------------------------------------------------------------
(defn try-closing
  "Run a thunk `f` that closes some resources. Catches exceptions and returns
  them wrapped in a [[ex-info]] with the message `error-msg`."
  [f error-msg]
  (try
    (f)
    (catch Exception e
      (ex-info error-msg {} e))))


(defn close-sse!
  "Closing a sse-gen is a 2 steps process.
  1. close io resources calling the `close-io!` thunk.
  2. call the sse-gen's `on-close` callback by calling the `on-close!` thunk.

  Both thunk are called using [[try-closing]] to capture exceptions.
  This function returns either true when all thunks are exceptions free or an 
  exception created with [[ex-info]] that contains the thunks exceptions in it's
  data."
  [close-io! on-close!]
  (let [io-close-res (try-closing close-io! "Error closing the output stream")
        on-close-res (try-closing on-close! "Error calling the on close callback.")
        errors (filterv #(instance? Exception %)
                        [io-close-res on-close-res])]
    (if (seq errors)
      (ex-info "Error closing the sse-gen." {:errors errors})
      true)))


(def on-exception
  "SSE option key:

  Callback that will be called when an exception is thrown sending an event.
  The signature must be (fn on-exception [sse e ctx]).
  Args:
  - `sse-gen`: the SSEGenrator
  - `e`: the exception
  - `ctx`: context information about the exception.

  The return value determines the behavior of the generator, a truthy value
  will tell the genrator to close itself.

  Defaults to [[default-on-exception]] which will close the `sse-gen` on
  IOException and rethrow otherwise."
  :on-exception)


(defn default-on-exception
  "Default on-exception callback, it returns true on [[IOException]] which
  closes,the generator. It rethrows otherwise."
  [_sse e ctx]
  (if (instance? IOException e)
    true
    (throw (ex-info "Error sending SSE event." ctx e))))

