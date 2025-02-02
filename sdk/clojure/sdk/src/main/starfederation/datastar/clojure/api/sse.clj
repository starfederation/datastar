(ns starfederation.datastar.clojure.api.sse
  (:require
    [starfederation.datastar.clojure.api.common :as common]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.protocols :as p]
    [starfederation.datastar.clojure.utils :as u])
  (:import
    java.lang.Appendable))


(def SSE-headers-1
  {"Cache-Control" "nocache"
   "Connection"    "keep-alive"
   "Content-Type"  "text/event-stream"})


(def SSE-headers-2+
  {"Cache-Control" "nocache"
   "Content-Type"  "text/event-stream"})


(defn headers
  "Returns sse headers given a `ring-request`, more specificaly given the
  `:protocol` key from that request."
  [ring-request]
  (let [protocol (:protocol ring-request)]
    (if (or
          (nil? protocol)
          (= "HTTP/1.1" protocol))
      SSE-headers-1
      SSE-headers-2+)))



;; -----------------------------------------------------------------------------
;; SSE prefixes and constants
;; -----------------------------------------------------------------------------
(def ^:private event-line-prefix "event: ")
(def ^:private id-line-prefix    "id: ")
(def ^:private retry-line-prefix "retry: ")
(def ^:private data-line-prefix  "data: ")

(def ^:private new-line     "\n")
(def ^:private end-event    (str new-line new-line))


;; -----------------------------------------------------------------------------
;; Appending to a buffer
;; -----------------------------------------------------------------------------
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
;; -----------------------------------------------------------------------------
(defn- append-event-type! [buffer event-type]
  (append-line! buffer event-line-prefix event-type
                "Failed to write event type." :event-type))

;; -----------------------------------------------------------------------------
;; Appending event opts
;; -----------------------------------------------------------------------------
(def ^:private add-event-id? u/not-empty-string?)

(defn- add-retry-duration? [d]
  (and d
       (> d 0)
       (not= d consts/default-sse-retry-duration)))


(defn- append-opts! [buffer {event-id common/id retry-duration common/retry-duration}]
  (when (add-event-id? event-id)
    (append-line! buffer id-line-prefix event-id
                  "Failed to write event id" common/id))

  (when (add-retry-duration? retry-duration)
    (append-line! buffer retry-line-prefix retry-duration
                  "Failed to write retry" common/retry-duration)))


;; -----------------------------------------------------------------------------
;; Appending event data
;; -----------------------------------------------------------------------------
(defn- append-data-lines! [buffer data-lines]
  (doseq [l data-lines]
    (append-line! buffer data-line-prefix l "Failed to write data." :data-line)))


;; -----------------------------------------------------------------------------
;; Append end event
;; -----------------------------------------------------------------------------
(defn- append-end-event! [buffer]
  (try
    (append! buffer end-event)
    (catch Exception e
      (throw (ex-info "Failed to write new lines."
                      {}
                      e)))))


;; -----------------------------------------------------------------------------
;; Public api
;; -----------------------------------------------------------------------------
(defn write-event!
  "Appends and event to an java.lang.Appendable buffer."
  [buffer event-type data-lines opts]
  (doto buffer
    (append-event-type! event-type)
    (append-opts! opts)
    (append-data-lines! data-lines)
    (append-end-event!)))



(defn send-event!
  "Wrapper around the [p/send-event!] function.
  It provides multiple arities and defaults options."
  ([sse-gen event-type data-lines]
   (send-event! sse-gen event-type data-lines {}))
  ([sse-gen event-type data-lines opts]
   (let [id (common/id opts "")
         retry-duration (common/retry-duration opts consts/default-sse-retry-duration)]
     (u/assert (string? id))
     (u/assert (number? retry-duration))
     (p/send-event! sse-gen
                    event-type
                    data-lines
                    {common/id id
                     common/retry-duration retry-duration}))))

