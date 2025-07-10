(ns starfederation.datastar.clojure.adapter.http-kit2
  (:require
    [org.httpkit.server :as hk-server]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.adapter.http-kit.impl :as impl]
    [starfederation.datastar.clojure.utils :refer [def-clone]]))


(def-clone on-open ac/on-open)
(def-clone on-close ac/on-close)
(def-clone on-exception ac/on-exception)
(def-clone default-on-exception ac/default-on-exception)


(def-clone write-profile ac/write-profile)

(def-clone basic-profile                impl/basic-profile)
(def-clone buffered-writer-profile      ac/buffered-writer-profile)
(def-clone gzip-profile                 ac/gzip-profile)
(def-clone gzip-buffered-writer-profile ac/gzip-buffered-writer-profile)


(defn- as-channel
  "
  Replacement for [[hk-server/as-channel]] that doesn't deal with websockets
  and doen't call `on-open` itself.

  `on-open` is meant to be called by either a middleware or an interceptor on the return.
  "
  [ring-req {:keys [on-close on-open init]}]

  (when-let [ch (:async-channel ring-req)]

    (when-let [f init]     (f ch))
    (when-let [f on-close] (org.httpkit.server/on-close ch (partial f ch)))

    {:body ch ::on-open on-open}))


(defn ->sse-response
  "Make a Ring like response that will start a SSE stream.

  The status code and the the SSE specific headers are not sent automatically.
  You need to use either [[start-responding-middleware]] or
  [[start-responding-interceptor]].

  Note that the SSE connection stays opened util you close it.

  General options:
  - `:status`: status for the HTTP response, defaults to 200.
  - `:headers`: ring headers map to add to the response.
  - [[on-open]]: mandatory callback called when the generator is ready to send.
  - [[on-close]]: callback called when the underlying Http-kit AsyncChannel is
    closed. It receives a second argument, the `:status-code` value we get from
    the closing AsyncChannel.
  - [[on-exception]]: callback called when sending a SSE event throws.
  - [[write-profile]]: write profile for the connection.
    Defaults to [[basic-profile]]

  SDK provided write profiles:
  - [[basic-profile]]
  - [[buffered-writer-profile]]
  - [[gzip-profile]]
  - [[gzip-buffered-writer-profile]]

  You can also take a look at the `starfederation.datastar.clojure.adapter.common`
  namespace if you want to write your own profiles.
  "
  [ring-request {:keys [status] :as opts}]
  {:pre [(ac/on-open opts)]}
  (let [on-open-cb (ac/on-open opts)
        on-close-cb (ac/on-close opts)
        future-send! (promise)
        future-gen (promise)]
    (assoc
      (as-channel ring-request
        {:on-open
         (fn [ch]
           (let [send! (impl/->send! ch opts)
                 sse-gen (impl/->sse-gen ch send! opts)]
             (deliver future-gen sse-gen)
             (deliver future-send! send!)
             (on-open-cb sse-gen)))

         :on-close
         (fn [_ status]
           (let [closing-res
                 (ac/close-sse!
                  #(when-let [send! (deref future-send! 0 nil)] (send!))
                  #(when on-close-cb
                     (on-close-cb (deref future-gen 0 nil) status)))]
             (if (instance? Exception closing-res)
               (throw closing-res)
               closing-res)))})
      :status (or status 200)
      :headers (ac/headers ring-request opts)
      ::datastar-sse-response true)))


(defn start-responding!
  "Function that takes a ring response map and sends HTTP status & headers using
  the [[AsyncChannel]] that should be in the body if the
  `::datastar-sse-response` key is present."
  [response]
  (if (::datastar-sse-response response)
    (let [{on-open ::on-open
           ch      :body} response
          response (dissoc response :body ::on-open ::datastar-sse-response)]
      (hk-server/send! ch response false)
      (on-open ch))
    response))



(defn wrap-start-responding
  "Middleware necessary to use in conjunction with [[->sse-response]].

  It will check if the response is a datastar-sse-response
  (created with [[->sse-response]]). In this case it will send the initial
  response containing headers and status code, then call `on-open`."
  [handler]
  (fn
    ([req]
     (let [response (handler req)]
       (start-responding! response)
       response))
    ([req respond _raise]
     (let [response (handler req)]
       (start-responding! response)
       (respond response)))))


(def start-responding-middleware
  "Reitit middleware map for [[wrap-start-responding]]."
  {:name ::start-responding
   :wrap wrap-start-responding})


(def start-responding-interceptor
  "
  Interceptor necessary to use in conjunction with [[->sse-response]].

  In the `:leave` fn, it will check if the response is a datastar-sse-response
  (created with [[->sse-response]]). In this case it will send the initial
  response containing headers and status code, then call `on-open`."
  {:name ::start-responding
   :leave (fn [ctx]
            (let [response (:response ctx)]
              (start-responding! response)
              ctx))})

