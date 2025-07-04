(ns user
  (:require
    [clojure.repl.deps :as crdeps]
    [clojure+.hashp    :as hashp]
    [clj-reload.core   :as reload]
    [malli.dev         :as mdev]))


(alter-var-root #'*warn-on-reflection* (constantly true))


(hashp/install!)

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
      sort)

  (require '[clojure.tools.build.api :as b])
  b/write-pom
  b/copy-dir)
