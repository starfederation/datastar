(ns starfederation.datastar.clojure.adapter.ring-jetty
  (:require
    [starfederation.datastar.clojure.adapter.ring-jetty.impl :as impl]
    [starfederation.datastar.clojure.api.sse :as sse]))


(defn ->sse-response
  "Returns a ring response with status 200, specific SSE headers merged
  with the provided ones and the body is a sse generator implementing
  `ring.core.protocols/StreamableResponseBody`.

  Note that the SSE connections stays opened until you close it in async mode.
  In sync mode, the connection is closed automatically when the handler is
  done running.

  Opts:
  - `:headers`: Ring headers map to add to the response
  - `:on-open`: Mandatory callback (fn [sse-gen] ...) called when the generator
    is ready to send.
  - `:on-close`: callback (fn [sse-gen] ...) called right after the generator
    has closed it's connection."
  [ring-request {:keys [headers on-open on-close]}]
  {:pre [(identity on-open)]}
  (let [sse-gen (impl/->sse-gen on-open on-close)]
    {:status 200
     :headers (merge headers (sse/headers ring-request))
     :body sse-gen}))

