(ns starfederation.datastar.clojure.adapter.http-kit
  (:require
    [org.httpkit.server :as hk-server]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.adapter.http-kit.impl :as impl]
    [starfederation.datastar.clojure.utils :refer [def-clone]]))


(def-clone on-exception ac/on-exception)
(def-clone default-on-exception ac/default-on-exception)


(def-clone write-profile ac/write-profile)

(def-clone basic-profile                impl/basic-profile)
(def-clone buffered-writer-profile      ac/buffered-writer-profile)
(def-clone gzip-profile                 ac/gzip-profile)
(def-clone gzip-buffered-writer-profile ac/gzip-buffered-writer-profile)


(defn ->sse-response
  "Make a Ring like response that will start a SSE stream.

  The status code and the the SSE specific headers are sent automatically
  before `on-open` is called.

  Note that the SSE connection stays opened util you close it.

  General options:
  - `:status`: Status for the HTTP response, defaults to 200
  - `:headers`: Ring headers map to add to the response.
  - `:on-open`: Mandatory callback `(fn [sse-gen] ...)` called when the
    generator is ready to send.
  - `:on-close`: callback `(fn [sse-gen status-code]...)` called when the
    underlying Http-kit AsyncChannel is closed.
  - [[on-exception]]: callback called when sending a SSE event throws
  - [[write-profile]]: write profile for the connection
    defaults to [[basic-profile]]

  When it comes to write profiles, the SDK provides:
  - [[basic-profile]]
  - [[buffered-writer-profile]]
  - [[gzip-profile]]
  - [[gzip-buffered-writer-profile]]

  You can also take a look at the `starfederation.datastar.clojure.adapter.common`
  namespace if you want to write your own profiles.
  "
  [ring-request {:keys [on-open on-close] :as opts}]
  (let [future-send! (promise)
        future-gen (promise)]
    (hk-server/as-channel ring-request
      {:on-open
       (fn [ch]
         (impl/send-base-sse-response! ch ring-request opts)
         (let [send! (impl/->send! ch opts)
               sse-gen (impl/->sse-gen ch send! opts)]
           (deliver future-gen sse-gen)
           (deliver future-send! send!)
           (on-open sse-gen)))

       :on-close
       (fn [_ status]
         (let [closing-res
               (ac/close-sse!
                #(when-let [send! (deref future-send! 0 nil)] (send!))
                #(when on-close
                   (on-close (deref future-gen 0 nil) status)))]
           (if (instance? Exception closing-res)
             (throw closing-res)
             closing-res)))})))
