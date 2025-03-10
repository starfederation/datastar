(ns starfederation.datastar.clojure.adapter.http-kit
  (:require
    [org.httpkit.server :as hk-server]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.adapter.http-kit.impl :as impl]
    [starfederation.datastar.clojure.utils :refer [def-clone]]))


(def-clone buffer-size ac/buffer-size)
(def-clone hold-write-buff? ac/hold-write-buff?)
(def-clone gzip? ac/gzip?)
(def-clone gzip-buffer-size ac/gzip-buffer-size)
(def-clone charset ac/charset)


(defn ->sse-response
  "Make a Ring like response that works with Http-kit.

  An empty response containing a 200 status code, the the SSE specific headers
  are sent automatically before `on-open` is called.

  Note that the SSE connection stays opened util you close it.

  General options:
  - `:status`: Status for the HTTP response, defaults to 200
  - `:headers`: Ring headers map to add to the response.
  - `:on-open`: Mandatory callback `(fn [sse-gen] ...)` called when the
    generator is ready to send.
  - `:on-close`: callback `(fn [sse-gen status-code]...)` called when the
    underlying Http-kit AsyncChannel is closed.


  SSE advanced options:
  see:
  - [[buffer-size]]
  - [[hold-write-buff?]]
  - [[gzip?]]
  - [[gzip-buffer-size]]
  - [[charset]]

  Note that [hold-write-buff?]] only works when [[gzip?]] is true. It is always
  considered false otherwise.

  Same for [[charset]], outside of compression the encoding
  of the events will be the default encoding of java strings.
  "
  [ring-request {:keys [on-open on-close] :as opts}]
  (let [future-send! (promise)
        future-gen (promise)]
    (hk-server/as-channel ring-request
      {:on-open
       (fn [ch]
         (impl/send-base-sse-response! ch ring-request opts)
         (let [send! (impl/->send! ch opts)
               sse-gen (impl/->sse-gen ch send!)]
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
