(ns starfederation.datastar.clojure.sdk-test.main
  (:require
    [ring.adapter.jetty :as jetty]
    [starfederation.datastar.clojure.sdk-test.core :as c])
  (:import
    org.eclipse.jetty.server.Server))



(defonce !jetty-server (atom nil))


(def default-server-opts {:port 8080
                          :join? false})

(defn start! [handler & {:as opts}]
  (let [opts (merge default-server-opts opts)]
    (println "Starting server port" (:port opts))
    (jetty/run-jetty handler opts)))


(defn stop! [server]
  (.stop ^Server server))


(defn reboot-jetty-server! [handler & {:as opts}]
  (swap! !jetty-server
         (fn [server]
           (when server
             (stop!  server))
           (start! handler opts))))


(comment
  (reboot-jetty-server! #'c/handler))

(defn -main [& _]
  (start! c/handler {:join? true}))
