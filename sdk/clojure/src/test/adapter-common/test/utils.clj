(ns test.utils
  (:require
    [charred.api :as charred])
  (:import
    java.io.StringWriter
    java.net.ServerSocket))

;; -----------------------------------------------------------------------------
;; JSON helpers
;; -----------------------------------------------------------------------------
(def ^:private bufSize 1024)
(def read-json (charred/parse-json-fn {:async? false :bufsize bufSize}))

(def ^:private write-json* (charred/write-json-fn {}))

(defn- write-json [s]
  (let [out (StringWriter.)]
    (write-json* out s)
    (.toString out)))

(comment
  (-> {"1" 2}
      (write-json)
      (read-json))
  := {"1" 2})


;; -----------------------------------------------------------------------------
;; Http servers helpers
;; -----------------------------------------------------------------------------
(defn free-port! []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))


(defn url [port path]
  (format "http://localhost:%s/%s" port path))



(defn sanitize-opts [opts]
  (-> opts
      (update :port #(or % (free-port!)))
      (dissoc :start! :stop!)))


(defmacro with-server
  "Setup a server

  Opts:
  - `:start!`: mandatory
  - `:stop!`: mandatory "
  [server-name handler opts & body]
  `(let [opts# ~opts
         {start!# :start!
          stop!#  :stop!} opts#
         sanitized-opts# (sanitize-opts opts#)
         ~server-name (start!# ~handler sanitized-opts#)]
     (try
       ~@body
       (finally
         (stop!# ~server-name)))))


(comment
  (macroexpand-1
    '(with-server serv h {:port 123456}
       (do1)
       (do2))))

