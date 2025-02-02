(ns tasks
  (:require
    [babashka.tasks :as t]
    [clojure.string :as string]
    [clojure.edn :as edn]))



(defn prep-libs []
  (t/clojure
    "-X:deps prep"))

(defn classpath [aliases]
  (apply str "-M" aliases))

(defn base-cli-invocation [aliases main]
  (str (classpath aliases)
       " -m "
       main))

(def dev-aliases
  [:test
   :repl
   :http-kit
   :ring-jetty
   :malli-schemas])

(defn dev []
 (t/clojure
   (str (base-cli-invocation dev-aliases 'nrepl.cmdline)
        " --middleware \"[cider.nrepl/cider-middleware]\"")))


;; -----------------------------------------------------------------------------
;; Helpers tasks for tests
; -----------------------------------------------------------------------------
(defn deps-aliases []
  (-> "deps.edn"
      slurp
      edn/read-string
      :aliases))


(def all-aliases (delay (deps-aliases)))


(defn named-paths->dirs [as]
  (->> as
       (mapcat #(get @all-aliases %))
       (map #(str "-d " %))))


(defn lazytest-invocation [aliases named-paths args]
  (string/join " "
    (concat [(base-cli-invocation aliases 'lazytest.main)]
            (named-paths->dirs named-paths)
            args
            *command-line-args*)))


(defn lazytest [aliases paths-aliases & args]
  (t/clojure
    (lazytest-invocation (into [:test] aliases)
                         paths-aliases
                         args)))


