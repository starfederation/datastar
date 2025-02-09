(ns example.server
  (:require
    [example.core :as c]
    [ring.adapter.jetty :as jetty])
  (:import
    org.eclipse.jetty.server.Server))




(defonce !jetty-server (atom nil))


(defn start! [handler & {:as opts}]
  (jetty/run-jetty handler
                   (merge {:port 8080 :join? false}
                          opts)))


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
