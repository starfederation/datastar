(ns starfederation.datastar.clojure.api.sse
  (:require
    [starfederation.datastar.clojure.api.common :as common]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u])
  (:import
    java.lang.Appendable))

;; -----------------------------------------------------------------------------
;; HTTP headers management
;; -----------------------------------------------------------------------------
(def base-SSE-headers
  {"Cache-Control" "no-cache"
   "Content-Type"  "text/event-stream"})


(defn add-keep-alive? [ring-request]
  (let [protocol (:protocol ring-request)]
    (or (nil? protocol)
        (neg? (compare protocol "HTTP/1.1")))))

(comment
  (add-keep-alive? {:protocol "HTTP/0.9"})
  (add-keep-alive? {:protocol "HTTP/1.0"})
  (add-keep-alive? {:protocol "HTTP/1.1"})
  (add-keep-alive? {:protocol "HTTP/2"}))


(defn headers
  "Returns headers for a SSE response. It adds specific SSE headers based on the
  HTTP protocol version found in the `ring-request`and the gzip content type
  if necessary.

  Options:
  - `:headers`: custom headers for the response
 
  The SSE headers this function provides can be overriden by the optional ones.
  Be carreful with the following headers:

  - \"Cache-Control\"
  - \"Content-Type\"
  - \"Connection\"
  "
  [ring-request & {:as opts}]
  (-> (transient {})
      (u/merge-transient! base-SSE-headers)
      (cond->
        (add-keep-alive? ring-request) (assoc! "Connection" "keep-alive",))
      (u/merge-transient! (:headers opts))
      persistent!))


;; -----------------------------------------------------------------------------
;; Assembling SSE event text
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; SSE prefixes and constants
(def ^:private event-line-prefix "event: ")
(def ^:private id-line-prefix    "id: ")
(def ^:private retry-line-prefix "retry: ")
(def ^:private data-line-prefix  "data: ")

(def ^:private new-line     "\n")
(def ^:private end-event    new-line)


;; -----------------------------------------------------------------------------
;; Appending to a buffer
(defn- append! [^Appendable a v]
  (.append a (str v)))


(defn- append-line! [buffer prefix line error-msg error-key]
  (try
    (doto buffer
      (append! prefix)
      (append! line)
      (append! new-line))
    (catch Exception e
      (throw (ex-info error-msg
                      {error-key line} e)))))

;; -----------------------------------------------------------------------------
;; Appending event type
(defn- append-event-type! [buffer event-type]
  (append-line! buffer event-line-prefix event-type
                "Failed to write event type." :event-type))

;; -----------------------------------------------------------------------------
;; Appending event opts
(defn- append-opts! [buffer {event-id common/id retry-duration common/retry-duration}]
  (when event-id
    (append-line! buffer id-line-prefix event-id
                  "Failed to write event id" common/id))

  (when retry-duration
    (append-line! buffer retry-line-prefix retry-duration
                  "Failed to write retry" common/retry-duration)))

;; -----------------------------------------------------------------------------
;; Appending event data
(defn- append-data-lines! [buffer data-lines]
  (doseq [l data-lines]
    (append-line! buffer data-line-prefix l "Failed to write data." :data-line)))

;; -----------------------------------------------------------------------------
;; Append end event
(defn- append-end-event! [buffer]
  (try
    (append! buffer end-event)
    (catch Exception e
      (throw (ex-info "Failed to write new lines."
                      {}
                      e)))))

;; -----------------------------------------------------------------------------
;; Public api
(defn write-event!
  "Appends and event to an java.lang.Appendable buffer."
  [appendable event-type data-lines opts]
  (doto appendable
    (append-event-type! event-type)
    (append-opts! opts)
    (append-data-lines! data-lines)
    (append-end-event!)))


(def ^:private keep-event-id? u/not-empty-string?)

(defn- keep-retry-duration? [d]
  (and d
       (> d 0)
       (not= d consts/default-sse-retry-duration)))


(defn- rework-options
  "Standardize opts values and decide whether to keep them or not:
  - if the id is an empty string it is thrown away
  - if the retry duration is 0 or datastars default it is thrown away

  This function asserts that the id must be a string and the retry duration a
  number."
  [opts]
  (let [id (common/id opts "")
        retry-duration (common/retry-duration opts consts/default-sse-retry-duration)]
    (u/assert (string? id))
    (u/assert (number? retry-duration))
    {common/id (and (keep-event-id? id) id)

     common/retry-duration (and (keep-retry-duration? retry-duration)
                                retry-duration)}))

(defn send-event!
  "Wrapper around the [p/send-event!] function.
  It provides multiple arities and defaults options."
  ([sse-gen event-type data-lines]
   (p/send-event! sse-gen event-type data-lines {}))
  ([sse-gen event-type data-lines opts]
   (p/send-event! sse-gen
                  event-type
                  data-lines
                  (rework-options opts))))
