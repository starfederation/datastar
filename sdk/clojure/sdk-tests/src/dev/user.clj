(ns user
  (:require
    [clj-reload.core :as reload]))


(alter-var-root #'*warn-on-reflection* (constantly true))

;(rcf/enable!)


(reload/init
  {:no-reload ['user]})


(defn reload! []
  (reload/reload))




(defn clear-terminal! []
  (binding [*out* (java.io.PrintWriter. System/out)]
    (print "\033c")
    (flush)))


