(ns starfederation.datastar.clojure.adapter.http-kit
  (:require
    [org.httpkit.server :as hk-server]
    [starfederation.datastar.clojure.adapter.http-kit.impl :as impl]))

(defn ->sse-response
  "Make a Ring like response that works with HTTP-Kit.

  An empty response containing a 200 status code, the
  `:headers`, and the SSE specific headers are sent
  automatically before `on-open` is called.

  Note that the SSE connection stays opened util you close it.

  Opts:
  - `:status`: Status for the HTTP response, defaults to 200
  - `:headers`: Ring headers map to add to the response.
  - `:on-open`: Mandatory callback (fn [sse-gen] ...) called when the
    generator is ready to send.
  - `:on-close`: callback (fn [sse-gen status-code])

  The callback are based on the  HTTP-Kit channel ones, adding the sse
  generator as the second parameter."
  [ring-request {:keys [on-open on-close] :as opts}]
  (let [future-gen (promise)
        response (hk-server/as-channel ring-request
                   {:on-open (fn [ch]
                               (impl/send-base-sse-response! ch ring-request opts)
                               (let [sse-gen (impl/->sse-gen ch)]
                                 (deliver future-gen sse-gen)
                                 (on-open sse-gen)))
                    :on-close (fn [_ status-code]
                                (when on-close
                                  (on-close (deref future-gen 0 nil)
                                            status-code)))})]
    response))

