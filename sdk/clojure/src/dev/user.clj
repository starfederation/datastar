(ns user
  (:require
    [clojure.repl.deps :as crdeps]
    [clj-reload.core :as reload]
    [malli.dev :as mdev]))


(alter-var-root #'*warn-on-reflection* (constantly true))


(reload/init
  {:no-reload ['user]})


(defn reload! []
  (reload/reload))


(defn clear-terminal! []
  (binding [*out* (java.io.PrintWriter. System/out)]
    (print "\033c")
    (flush)))


(defmacro force-out [& body]
  `(binding [*out* (java.io.OutputStreamWriter. System/out)]
     ~@body))
 

(comment
  (mdev/start! {:exception true})
  (mdev/stop!)
  (reload!)
  *e
  (crdeps/sync-deps)

  (-> (System/getProperties)
      keys
      sort))


