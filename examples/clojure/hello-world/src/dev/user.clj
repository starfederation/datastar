(ns user
  (:require
    [clj-reload.core :as reload]))


(alter-var-root #'*warn-on-reflection* (constantly true))


(reload/init
  {:no-reload ['user]})


(defn reload! []
  (reload/reload))


(comment
  (reload!)
  *e)


