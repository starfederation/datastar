(ns example.utils
  (:require
    [charred.api :as charred]
    [starfederation.datastar.clojure.api :as d*]))


(def ^:private bufSize 1024)
(def read-json (charred/parse-json-fn {:async? false :bufsize bufSize}))

(defn get-signals [req]
  (-> req d*/get-signals read-json))


