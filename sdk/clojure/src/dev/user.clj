(ns user
  (:require
    [clojure.repl.deps :as crdeps]
    [clj-reload.core :as reload]
    [malli.dev :as mdev]))
;    [hyperfiddle.rcf :as rcf]))


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



(comment
  (mdev/start!)
  (mdev/stop!)
  (reload!)
  *e
  (crdeps/sync-deps)

  (-> (System/getProperties)
      keys
      sort))


