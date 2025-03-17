(ns starfederation.datastar.clojure.adapter.ring
  (:require
    [starfederation.datastar.clojure.adapter.ring.impl :as impl]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.utils :refer [def-clone]]))


(def-clone on-open ac/on-open)
(def-clone on-close ac/on-close)
(def-clone on-exception ac/on-exception)
(def-clone default-on-exception ac/default-on-exception)


(def-clone write-profile ac/write-profile)

(def-clone basic-profile                ac/basic-profile)
(def-clone buffered-writer-profile      ac/buffered-writer-profile)
(def-clone gzip-profile                 ac/gzip-profile)
(def-clone gzip-buffered-writer-profile ac/gzip-buffered-writer-profile)


(defn ->sse-response
  "Returns a ring response that will start a SSE stream.

  The status code will be either 200 or the user provided one.
  Specific SSE headers are set automatically, the user provided ones will be
  merged. The response body is a sse generator implementing
  `ring.core.protocols/StreamableResponseBody`.

  In sync mode, the connection is closed automatically when the handler is
  done running. You need to explicitely close it in rinc async.

  Opts:
  - `:status`: status for the HTTP response, defaults to 200
  - `:headers`: Ring headers map to add to the response
  - [[on-open]]: Mandatory callback (fn [sse-gen] ...) called when the generator
    is ready to send.
  - [[on-close]]: callback (fn [sse-gen] ...) called right after the generator
    has closed it's connection.
  - [[on-exception]]: callback called when sending a SSE event throws
  - [[write-profile]]: write profile for the connection
    defaults to [[basic-profile]]

  - `:on-open`: deprecated in favor of [[on-open]]
  - `:on-close`: deprecated in favor of [[on-close]]

  When it comes to write profiles, the SDK provides:
  - [[basic-profile]]
  - [[buffered-writer-profile]]
  - [[gzip-profile]]
  - [[gzip-buffered-writer-profile]]

  You can also take a look at the `starfederation.datastar.clojure.adapter.common`
  namespace if you want to write your own profiles.
  "
  [ring-request {:keys [status] :as opts}]
  {:pre [(ac/get-on-open opts)]}
  (let [sse-gen (impl/->sse-gen)]
    {:status (or status 200)
     :headers (ac/headers ring-request opts)
     :body sse-gen
     ::impl/opts opts}))

