(ns ^{:doc "
Namespace containing the shared code for each adapter.

It contains helpers for working with output streams and assembling SSE sending
function following several strategies.
      "}
  starfederation.datastar.clojure.adapter.common
  (:refer-clojure :exclude [flush])
  (:require
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.utils :as u])
  (:import
    [java.io BufferedWriter Flushable OutputStream OutputStreamWriter Writer]
    [java.nio.charset Charset StandardCharsets]
    java.util.zip.GZIPOutputStream))


;; -----------------------------------------------------------------------------
;; Advanced SSE options
;; -----------------------------------------------------------------------------
(def buffer-size
  "SSE option:

  Size of the [[BufferedWriter]] buffer or initial size for the StringBuilder
  constructing the SSE event's text (see [[hold-write-buff?]])

  Defaults to either java's default for [[BufferedWriter]]
  or [[default-write-buffer-size]] for initial StringBuilder capacity."
  :d*.sse/buffer-size)


(def hold-write-buff?
  "SSE option:

  Whether to keep a write buffer around
  - if true: the constructed `write!` fn hold onto a [[BufferedWriter]] for the
    duration of the SSE connection and writes all events to it.
  - if false: for each event sent a [[StringBuilder]] is used to write the
    event. This builder is then discarded when the event is sent.

  Defaults to false."
  :d*.sse/hold-write-buff?)


(def charset
  "SSE option:
  [[Charset]] controling the encoding of the SSE stream.

  Defaults to [[StandardCharsets/UTF_8]]"
  :d*.sse/charset)



(def gzip?
  "SSE option:

  Whether to compress the SSE event stream."
  :d*.sse/gzip?)


(def gzip-buffer-size
  "SSE option:
  Size of the buffer used by [[GZIPOutputStream]]
 
  Default to the GZIPOutputStream's default
  "
  :d*.sse/gzip-buffer-size)


;; -----------------------------------------------------------------------------
;; HTTP headers helper
;; -----------------------------------------------------------------------------
(defn headers
  " Same as [[sse/headers]] with the added responsibility to add the 
  Content-Encoding header when the `gzip?` option is true.
  "
  [ring-request & {:as opts}]
  (-> (transient {})
      (u/merge-transient! sse/base-SSE-headers)
      (cond->
        (sse/http1? ring-request) (assoc! "Connection"       "keep-alive",)
        (gzip? opts)              (assoc! "Content-Encoding" "gzip"))
      (u/merge-transient! (:headers opts))
      persistent!))


(comment
  (headers {:protocol "HTTP/2"} {})
  (headers {:protocol "HTTP/2"} {gzip? true}))

;; -----------------------------------------------------------------------------
;; OutputStream wrapping
;; -----------------------------------------------------------------------------
(defn ->gzip-os
  "Make a [[GZIPOutputStream]] an [[OutputStream]] `os`.

  Opts keys:
  - see [[gzip?]], if false returns `os`
  - see [[gzip-buffer-size]]"
  [^OutputStream os opts]
  (if-let [s (gzip-buffer-size opts)]
    (GZIPOutputStream. os s true)
    (GZIPOutputStream. os true)))


(defn ->os-writer
  "Make a [[OutputStreamWriter]] from another [[OutputStreamWriter]] `os`.

  Opts:
  - see [[charset]]
  "
  [^OutputStream os opts]
  (if-let [^Charset c (charset opts)]
    (OutputStreamWriter. os c)
    (OutputStreamWriter. os StandardCharsets/UTF_8)))


(defn ->buffered-writer
  "Make a [[BufferedWriter]] from an [[OutputStreamWriter]] `osw`.
  Opts
  - see [[buffer-size]]
  "
  [osw opts]
  (if-let [s (buffer-size opts)]
    (BufferedWriter. osw s)
    (BufferedWriter. osw)))


(defn wrap-output-stream
  "Wrap the outputstream use for an SSE connections with the following:

  - [[->gzip-os]]
  - [[->os-writer]]
  - [[->buffered-writer]]

  The options in the map control buffer size and wheter some wrapper is applied.
  See
  - [[gzip?]]
  - [[gzip-buffer-size]]
  - [[charset]]
  - [[hold-write-buff?]]
  - [[buffer-size]]
  "
  [os opts]
  (cond-> os
      (gzip? opts)            (->gzip-os opts)
      true                    (->os-writer opts)
      (hold-write-buff? opts) (->buffered-writer opts)))

;; -----------------------------------------------------------------------------
;; Writing utilities
;; -----------------------------------------------------------------------------
(defn build-event-str
  "Build the SSE event string using a [[StringBuilder]]."
  ^String [buffer-size event-type data-lines opts]
  (-> (StringBuilder. (int buffer-size))
      (sse/write-event! event-type data-lines opts)
      str))


(def default-write-buffer-size 8192)

(defn ->write-with-temp-buffer!
  "Make a function that will assemble a SSE event using a [[StringBuilder]] and
  then will write the resulting string to an [[OutputStreamWriter]].

  Flushes immediately after writing the event."
  [->sse-response-opts]
  (let [buffer-size (buffer-size ->sse-response-opts default-write-buffer-size)]
    (fn write-with-temp-buffer! [^Writer writer event-type data-lines opts]
      (let [event-str (build-event-str buffer-size event-type data-lines opts)]
        (.append writer event-str)))))


(defn write-to-buffered-writer!
  "Write a SSE event to a stream using a [[BufferedWriter]].

  Flushes immediately after writing the event."
  [^BufferedWriter writer event-type data-lines opts]
  (sse/write-event! writer event-type data-lines opts))


(defn flush
  "Flush a `java.io.Flushable`."
  [^Flushable f]
  (.flush f))


(defn ->write-machinery
  "Return a map containing the machinery to write SSE event based on an
  [[OutputStream]] `os` and a map of option.

  Option keys:
  - [[gzip?]]
  - [[gzip-buffer-size]]
  - [[charset]]
  - [[hold-write-buff?]]
  - [[buffer-size]]

  The returned map has 2 keys:
  - `:writer` the wrapped `os` using the [[wrap-output-stream]] fn.
  - `:write!` a function that know how to write to the wrapped stream.
  "
  [os ->sse-response-opts]
  {:writer (wrap-output-stream os ->sse-response-opts)
   :write! (if (hold-write-buff? ->sse-response-opts)
             write-to-buffered-writer!
             (->write-with-temp-buffer! ->sse-response-opts))})


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


