(ns examples.utils
  (:require
    [charred.api :as charred]
    [fireworks.core :refer [?]]
    [org.httpkit.server :as hk-server]
    [ring.adapter.jetty :as jetty]
    [starfederation.datastar.clojure.api :as d*])
  (:import
    org.eclipse.jetty.server.Server))


(defn clear-terminal! []
  (binding [*out* (java.io.PrintWriter. System/out)]
    (print "\033c")
    (flush)))




(def ^:private bufSize 1024)
(def read-json (charred/parse-json-fn {:async? false :bufsize bufSize}))



(defn get-signals [req]
  (-> req d*/get-signals read-json))


(defonce !hk-server (atom nil))


(defn reboot-hk-server! [handler]
  (swap! !hk-server
         (fn [server]
           (when server
             (hk-server/server-stop! server))
           (hk-server/run-server handler
                                 {:port 8080
                                  :legacy-return-value? false}))))


(defonce !jetty-server (atom nil))


(defn reboot-jetty-server! [handler & {:as opts}]
  (swap! !jetty-server
         (fn [server]
           (when server
             (.stop ^Server server))
           (jetty/run-jetty handler
                            (merge
                              {:port 8081
                               :join? false}
                              opts)))))




(defn ?req [req]
  (? (dissoc req :reitit.core/match :reitit.core/router)))
