(ns starfederation.datastar.clojure.adapter.ring
  (:require
    [starfederation.datastar.clojure.adapter.ring.impl :as impl]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.utils :refer [def-clone]]))


(def-clone buffer-size      ac/buffer-size)
(def-clone hold-write-buff? ac/hold-write-buff?)
(def-clone gzip?            ac/gzip?)
(def-clone gzip-buffer-size ac/gzip-buffer-size)
(def-clone charset          ac/charset)


(defn ->sse-response
  "Returns a ring response with status 200, specific SSE headers merged
  with the provided ones and whose body is a sse generator implementing
  `ring.core.protocols/StreamableResponseBody`.

  In sync mode, the connection is closed automatically when the handler is
  done running.

  Opts:
  - `:status`: status for the HTTP response, defaults to 200
  - `:headers`: Ring headers map to add to the response
  - `:on-open`: Mandatory callback (fn [sse-gen] ...) called when the generator
    is ready to send.
  - `:on-close`: callback (fn [sse-gen] ...) called right after the generator
    has closed it's connection.

  SSE advanced options:
  see:
  - [[buffer-size]]
  - [[hold-write-buff?]]
  - [[gzip?]]
  - [[gzip-buffer-size]]
  - [[charset]]
  "
  [ring-request {:keys [status on-open on-close] :as opts}]
  {:pre [(identity on-open)]}
  (let [sse-gen (impl/->sse-gen on-close)]
    {:status (or status 200)
     :headers (ac/headers ring-request opts)
     :body sse-gen
     ::impl/opts opts}))

