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
   :malli-schemas
   :sdk-brotli])

(defn arg->kw [s]
  (if (string/starts-with? s ":")
    (keyword (subs s 1))
    (keyword s)))


(defn dev [& aliases]
  (let [aliases (-> dev-aliases
                    (into aliases)
                    (into (map arg->kw *command-line-args*)))]
    (println "Starting Dev repl with aliases: " aliases)
    (t/clojure
      (str (base-cli-invocation aliases 'nrepl.cmdline)
           " --middleware \"[cider.nrepl/cider-middleware]\""))))


;; -----------------------------------------------------------------------------
;; Helpers tasks for tests
;; -----------------------------------------------------------------------------
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


;; -----------------------------------------------------------------------------
;; Build tasks
;; -----------------------------------------------------------------------------
(def sdk-dir                  "sdk")
(def sdk-adapter-http-kit-dir "sdk-adapter-http-kit")
(def sdk-adapter-ring-dir     "sdk-adapter-ring")
(def sdk-brotli-dir           "sdk-brotli")
(def sdk-malli-schemas-dir    "sdk-malli-schemas")

(def sdk-lib-dirs
  [sdk-dir
   sdk-adapter-ring-dir
   sdk-adapter-http-kit-dir
   sdk-brotli-dir
   sdk-malli-schemas-dir])


(defn lib-jar! [dir]
  (t/clojure {:dir dir} "-T:build jar"))


(defn lib-install! [dir]
  (t/clojure {:dir dir} "-T:build install"))


(defn lib-clean! [dir]
  (t/clojure {:dir dir} "-T:build clean"))


(defn lib-bump! [dir component]
  (when-not (contains? #{"major" "minor" "patch"} component)
    (println (str "ERROR: First argument must be one of: major, minor, patch. Got: " (or component "nil")))
    (System/exit 1))
  (t/shell {:dir dir} (str "neil version " component " --no-tag")))


(defn lib-set-version! [dir version]
  (t/shell {:dir dir} (str "neil version set " version " --no-tag")))


(defn lib-publish! [dir]
  (t/clojure {:dir dir} "-T:build deploy"))
