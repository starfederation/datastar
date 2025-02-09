(ns example.main
  (:require
    [example.core :as c]
    [example.server :as server]))


(defn -main [& _]
  (server/start! c/handler {:join? true}))
