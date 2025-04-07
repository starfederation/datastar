(ns test.common
  (:require
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [clojure.string :as string]
    [etaoin.api :as ea]
    [lazytest.core :as lt]
    [lazytest.extensions.matcher-combinators :as mc]
    [org.httpkit.client :as http]
    [starfederation.datastar.clojure.adapter.test :as test-gen]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.api.sse :as sse]
    [test.utils :as u]))


(def ^:dynamic *ctx* nil)


#_{:clj-kondo/ignore true}
(defn with-server-f
  "Http server around fixture.

  Opts:
  - `:start!`: fn starting a http server
  - `:stop!`: fn stoping the server
  - `:get-port`: get the port used by the running server
  - other: specific opts for the http server used
  "
  [handler opts]
  (lt/around [f]
    (let [get-port (:get-port opts)]
      (u/with-server s handler (dissoc opts :get-port)
        (binding [*ctx* (assoc *ctx* :port (get-port s))]
          (f))))))


(def default-drivers-config
  {:chrome {:headless true
            :args ["-incognito"]}
   :firefox {:headless true
             :args ["-private"]}})


(def custom-config
  "Config from \"test-resources/test.config.edn\".

  Keys:
  - `:drivers`: vector of driver types to run (kws like `:firefox`... from etaoin)
  - `:webdriver-opts`: map of drivers types to drivers opts
    (options passed to etaoin driver starting opts)
  "
  (-> "test.config.edn"
      io/resource
      slurp
      edn/read-string))


(def drivers-configs
  "Merge of the default drivers opts and the custom ones."
  (let [custom-opts (:webdriver-opts custom-config)]
    (reduce-kv
      (fn [acc type opts]
        (assoc acc type (merge opts
                               (get custom-opts type))))
      {}
      default-drivers-config)))


(defonce drivers
  (reduce
    (fn [acc driver]
      (assoc acc driver (delay (ea/boot-driver driver (get drivers-configs driver)))))
    {}
    (:drivers custom-config)))


(defn install-shutdown-hooks! []
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. (fn []
                               ; Killing web drivers
                               (println "Killing web drivers")
                               (doseq [d (vals drivers)]
                                 (when (realized? d)
                                   (try
                                     (ea/quit @d)
                                     (catch Exception _
                                       (println "Exception killing webdriver")))))

                               ; Killing agents
                               (println "Killing agents")
                               (shutdown-agents)))))

(defonce _ (install-shutdown-hooks!))

;; -----------------------------------------------------------------------------
;; Generic counters tests
;; -----------------------------------------------------------------------------
(defn run-counters! [driver]
  (let [{:keys [port]} *ctx*]
    (ea/go driver (u/url port "counters/"))

    (ea/click driver :increment-1)
    (ea/click driver :increment-1)
    (ea/click driver :decrement-2)
    (ea/click driver :increment-3)


    {:get (ea/get-element-text driver :counter1)
     :post (ea/get-element-text driver :counter2)
     :signal (ea/get-element-text driver :counter3)}))


(def expected-counters {:get "2"
                        :post "-1"
                        :signal "1"})


;; -----------------------------------------------------------------------------
;; form test
;; -----------------------------------------------------------------------------
(defn do-form! [driver msg button]
  (ea/go driver (u/url (:port *ctx*) "form"))
  (ea/fill driver :input-1 msg)
  (ea/click driver button)
  (ea/clear driver :input-1)
  (ea/get-element-text driver :form-result))


(defn run-form-test! [driver]
  {:get (do-form! driver "get" :get-form)
   :post (do-form! driver "post" :post-form)})


(def expected-form-vals
  {:get "get"
   :post "post"})

;; -----------------------------------------------------------------------------
;; Generic persistent connection tests
;; -----------------------------------------------------------------------------
(defn- ->persistent-sse-handler
  "Make a ring handler that puts a sse gen into an atom for use later.
  Counts down an latch to allow the tests to continue."
  [->sse-response !conn]
  (fn handler
    ([req]
     (->sse-response req
       {ac/on-open (fn [sse-gen]
                     (deliver !conn sse-gen))}))
    ([req respond _raise]
     (respond (handler req)))))


(defn setup-persistent-see-state
  "Setup of the persistent connection test.
  We put together an atom to store a sse generator, a countdown latch and a
  ring handler hooked to them."
  [->sse-response]
  (let [!conn (promise)
        handler (->persistent-sse-handler ->sse-response !conn)]
    {:!conn !conn
     :handler handler}))


#_{:clj-kondo/ignore true}
(defn persistent-sse-f
  "Fixture for the persistent sse test. A server is set up and the state
  needed to run the test (see [[setup-persistent-see-state]])."
  [->sse-response server-opts]
  (lt/around [f]
    (let [{:keys [get-port]} server-opts
          {:keys [!conn latch handler]} (setup-persistent-see-state ->sse-response)]
      (u/with-server server handler (dissoc server-opts :get-port)
        (binding [*ctx* {:port (get-port server)
                         :!conn !conn}]
          (f))))))


(defn persistent-see-send-events! [sse-gen]
  (d*/merge-fragment! sse-gen "1")
  (d*/merge-fragment! sse-gen "2"))


(defn run-persistent-sse-test! []
  (let [{:keys [port !conn]} *ctx*
        response (http/request {:url (u/url port "")})
        sse-gen (deref !conn 100 nil)]
    (when-not sse-gen
      (throw (ex-info "The handler did not deliver the persistent sse-gen." {})))
    (persistent-see-send-events! sse-gen)
    (d*/close-sse! sse-gen)
    (deref response 10 :error)))



(defn p-sse-status-ok? [response]
  (lt/expect (= (:status response) 200)))


(defn ->headers [req]
  (-> req
      sse/headers
      (update-keys (comp keyword string/lower-case))))

(def SSE-headers-1-dot-0 (->headers {}))
(def SSE-headers-1-dot-1 (->headers {:protocol "HTTP/1.1"}))
(def SSE-headers-2+      (->headers {:protocol "HTTP/2"}))


(defn p-sse-http1-headers-ok? [response]
  (lt/expect (mc/match? SSE-headers-1-dot-1 (:headers response))))


(def expected-p-sse-res-body
  (let [sse-gen (test-gen/->sse-gen)]
    (str (d*/merge-fragment! sse-gen "1")
         (d*/merge-fragment! sse-gen "2"))))


(defn p-sse-body-ok? [response]
  (lt/expect (= (:body response) expected-p-sse-res-body)))
