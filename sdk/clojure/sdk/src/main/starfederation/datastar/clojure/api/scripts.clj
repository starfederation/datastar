(ns starfederation.datastar.clojure.api.scripts
  (:require
    [clojure.string :as string]
    [starfederation.datastar.clojure.api.common :as common]
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.utils :as u]))


(defn add-auto-remove? [val]
  (common/add-boolean-option? consts/default-execute-script-auto-remove
                              val))

(defn- add-auto-remove?! [data-lines! ar]
  (common/add-opt-line!
    data-lines!
    add-auto-remove?
    consts/auto-remove-dataline-literal
    ar))


(defn- attributes->lines [m]
  (persistent!
    (reduce-kv
      (fn [acc k v]
        (conj! acc (str (name k) \space v)))
      (transient [])
      m)))


(defn- add-attributes? [attributes]
  (and attributes
       (not= attributes consts/default-execute-script-attributes)))

(defn- add-attributes! [data-lines! attributes]
  (cond-> data-lines!
    (add-attributes? attributes)
    (common/add-data-lines! consts/attributes-dataline-literal
                            (attributes->lines attributes))))



(defn- add-script! [data-lines! script-content]
  (cond-> data-lines!
    (u/not-empty-string? script-content)
    (common/add-data-lines! consts/script-dataline-literal
                            (string/split-lines script-content))))


(defn ->script [script-content opts]
  (u/transient-> []
    (add-attributes! (common/attributes opts))
    (add-auto-remove?! (common/auto-remove opts))
    (add-script! script-content)))


(comment
  (->script "console.log('hello')" {})
  := ["script console.log('hello')"]

  (->script "console.log('hello')"
              {common/auto-remove false})
  := ["autoRemove false" "script console.log('hello')"]


 
  (->script "console.log('hello')"
            {common/auto-remove false
             common/attributes {:type "module"}})
  := ["autoRemove false" "script console.log('hello')"]


  (->script "console.log('hello')\nconsole.log('world!!!')"
            {common/attributes {:type "module" :data-something 1}})
  := ["attributes type module"
      "attributes data-something 1"
      "script console.log('hello')"
      "script console.log('world!!!')"])


(defn execute-script! [sse-gen script-content opts]
  (try
    (sse/send-event! sse-gen
                     consts/event-type-execute-script
                     (->script script-content opts)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send script"
                      {:script script-content}
                      e)))))


