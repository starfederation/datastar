(ns starfederation.datastar.clojure.adapter.common-schemas
  (:require
    [malli.core :as m]
    [malli.util :as mu]
    [starfederation.datastar.clojure.adapter.common :as ac])
  (:import
    java.io.OutputStream
    java.nio.charset.Charset))


(defn output-stream? [o]
  (instance? OutputStream o))

(def output-stream-schema
  [:fn {:error/message "should be a java.io.OutputStream"}
   output-stream?])


(defn charset? [c]
  (instance? Charset c))

(def charset-schema
  [:fn {:error/message "should be a java.nio.charset.Charset"}
   charset?])



(def SSE-buffering-options
  [:map
   [ac/buffer-size :int]
   [ac/hold-write-buff? :boolean]
   [ac/charset charset-schema]
   [ac/gzip? :boolean]
   [ac/gzip-buffer-size :int]])


(m/=> starfederation.datastar.clojure.adapter.common/->write-machinery
      [:function
       [:-> output-stream-schema SSE-buffering-options :any]])


(def ->sse-response-http-options-schema
  [:map
   [:status number?]
   [:headers [:map-of :string [:or :string [:seqable :string]]]]])


(def ->sse-response-callbacks-options-schema
  [:map
   [:on-open fn?]
   [:on-close fn?]])


(def ->sse-response-options-schema
  (-> ->sse-response-http-options-schema
      (mu/merge ->sse-response-callbacks-options-schema)
      (mu/merge SSE-buffering-options)))

