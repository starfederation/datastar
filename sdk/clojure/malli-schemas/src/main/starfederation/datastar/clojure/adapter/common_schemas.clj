(ns starfederation.datastar.clojure.adapter.common-schemas
  (:require
    [malli.core :as m]
    [malli.util :as mu]
    [starfederation.datastar.clojure.adapter.common :as ac])
  (:import
    [java.io BufferedWriter OutputStream OutputStreamWriter Writer]
    java.nio.charset.Charset
    java.util.zip.GZIPOutputStream))


(defn output-stream? [o]
  (instance? OutputStream o))

(def output-stream-schema
  [:fn {:error/message "should be a java.io.OutputStream"}
   output-stream?])


(defn gzip-output-stream? [o]
  (instance? GZIPOutputStream o))

(def gzip-output-stream-schema
  [:fn {:error/message "should be a java.util.zip.GZIPOutputStream"}
   gzip-output-stream?])


(m/=> starfederation.datastar.clojure.adapter.common/->gzip-os
      [:function
       [:-> output-stream-schema      gzip-output-stream-schema]
       [:-> output-stream-schema :int gzip-output-stream-schema]])


(defn output-stream-writer? [o]
  (instance? OutputStreamWriter o))

(def output-stream-writer-schema
  [:fn {:error/message "should be a java.io.OutputStreamWriter"}
   output-stream-writer?])


(defn charset? [c]
  (instance? Charset c))

(def charset-schema
  [:fn {:error/message "should be a java.nio.charset.Charset"}
   charset?])


(m/=> starfederation.datastar.clojure.adapter.common/->os-writer
      [:function
       [:-> output-stream-schema                output-stream-writer-schema]
       [:-> output-stream-schema charset-schema output-stream-writer-schema]])


(defn buffered-writer? [o]
  (instance? BufferedWriter o))

(def buffered-writer-schema
  [:fn {:error/message "should be a java.io.BufferedWriter"}
   buffered-writer?])


(m/=> starfederation.datastar.clojure.adapter.common/->buffered-writer
      [:function
       [:-> output-stream-writer-schema      buffered-writer-schema]
       [:-> output-stream-writer-schema :int buffered-writer-schema]])


(defn writer? [x]
  (instance? Writer x))

(def writer-schema
  [:fn {:error/message "should be a java.io.Writer"}
   writer?])

(def wrap-output-stream-schema
  [:-> output-stream-schema writer-schema])

(def write-profile-schema
  (mu/optional-keys
    [:map
     [ac/wrap-output-stream wrap-output-stream-schema]
     [ac/write! fn?]
     [ac/content-encoding :string]]

    [ac/content-encoding]))

(def SSE-write-profile-opts
  (mu/optional-keys
    [:map
     [ac/write-profile write-profile-schema]]))


(def ->sse-response-http-options-schema
  (mu/optional-keys
    [:map
     [:status number?]
     [:headers [:map-of :string [:or :string [:seqable :string]]]]]
    [:status :headers]))


(def ->sse-response-callbacks-options-schema
  (mu/optional-keys
    [:map
     [ac/on-open fn?]
     [ac/on-close fn?]
     [ac/on-exception fn?]]
    [ac/on-close ac/on-exception]))


(def ->sse-response-options-schema
  (-> ->sse-response-http-options-schema
      (mu/merge ->sse-response-callbacks-options-schema)
      (mu/merge SSE-write-profile-opts)))

