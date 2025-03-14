(ns examples.utils
  (:require
    [charred.api :as charred]
    [fireworks.core :refer [?]]
    [starfederation.datastar.clojure.api :as d*]))


;; -----------------------------------------------------------------------------
;; Misc utils
;; -----------------------------------------------------------------------------
(defn clear-terminal! []
  (binding [*out* (java.io.PrintWriter. System/out)]
    (print "\033c")
    (flush)))


(defmacro force-out [& body]
  `(binding [*out* (java.io.OutputStreamWriter. System/out)]
     ~@body))
 

(def ^:private bufSize 1024)
(def read-json (charred/parse-json-fn {:async? false :bufsize bufSize}))



(defn get-signals [req]
  (-> req d*/get-signals read-json))


(defn rr [sym]
  (try
    (requiring-resolve sym)
    (catch Exception _
      nil)))

;; -----------------------------------------------------------------------------
;; httpkit server
;; -----------------------------------------------------------------------------
(defonce !hk-server (atom nil))

(def http-kit-run! (rr 'org.httpkit.server/run-server))
(def http-kit-stop! (rr 'org.httpkit.server/server-stop!))

(defn reboot-hk-server! [handler]
  (if-not http-kit-run!
    (println "http kit isn't in the classpath")
    (swap! !hk-server
           (fn [server]
             (when server
               (http-kit-stop! server))
             (http-kit-run! handler
                             {:port 8080
                              :legacy-return-value? false})))))

;; -----------------------------------------------------------------------------
;; ring jetty server
;; -----------------------------------------------------------------------------
(defonce !jetty-server (atom nil))

(def ring-jetty-run! (rr 'ring.adapter.jetty/run-jetty))


(defn reboot-jetty-server! [handler & {:as opts}]
  (if-not ring-jetty-run!
    (println "Ring jetty isn't in the classpath")
    (swap! !jetty-server
           (fn [server]
             (when server
               (.stop server))
             (ring-jetty-run! handler
                             (merge
                               {:port 8081
                                :join? false}
                               opts))))))

;; -----------------------------------------------------------------------------
;; rj9a server
;; -----------------------------------------------------------------------------
(defonce !rj9a-server (atom nil))

(def rj9a-run! (rr 'ring.adapter.jetty9/run-jetty))


(defn reboot-rj9a-server! [handler & {:as opts}]
  (if-not rj9a-run!
    (println "Ring jetty isn't in the classpath")
    (swap! !rj9a-server
           (fn [server]
             (when server
               (.stop server))
             (rj9a-run! handler
                        (merge
                          {:port 8082
                           :join? false}
                          opts))))))


(defn ?req [req]
  (? (dissoc req :reitit.core/match :reitit.core/router)))

