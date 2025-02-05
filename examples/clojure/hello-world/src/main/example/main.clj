(ns example.main
  (:require
    [example.core :as c]
    [example.server :as server]))


(defn -main [& _]
  (let [server (server/start! c/handler)]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                  (server/stop! server)
                                  (shutdown-agents))))))
