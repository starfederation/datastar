(ns starfederation.datastar.clojure.api.signals
  (:require
    [clojure.string :as string]
    [starfederation.datastar.clojure.api.common :as common]
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.utils :as u]))


;; -----------------------------------------------------------------------------
;; Merge signal
;; -----------------------------------------------------------------------------
(defn add-only-if-missing? [val]
  (common/add-boolean-option? consts/default-merge-signals-only-if-missing
                              val))

(defn- add-only-if-missing?! [data-lines! only-if-missing]
  (common/add-opt-line!
    data-lines!
    add-only-if-missing?
    consts/only-if-missing-dataline-literal
    only-if-missing))


(defn- add-merge-signals! [data-lines! signals]
  (cond-> data-lines!
    (u/not-empty-string? signals)
    (common/add-data-lines! consts/signals-dataline-literal
                            (string/split-lines signals))))


(defn ->merge-signals [signals opts]
  (u/transient-> []
    (add-only-if-missing?! (common/only-if-missing opts))
    (add-merge-signals! signals)))


(comment
  (->merge-signals "{some json}\n{some other json}" {})
  := ["signals {some json}"
      "signals {some other json}"]

  (->merge-signals "{some json}\n{some other json}"
                   {common/only-if-missing :toto})
  := ["onlyIfMissing true"
      "signals {some json}"
      "signals {some other json}"])


 
(defn merge-signals! [sse-gen signals-content opts]
  (try
    (sse/send-event! sse-gen
                     consts/event-type-merge-signals
                     (->merge-signals signals-content opts)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send merge signals"
                      {:signals signals-content}
                      e)))))


;; -----------------------------------------------------------------------------
;; Remove signals
;; -----------------------------------------------------------------------------
(defn add-remove-signals-paths! [data-lines! paths]
  (common/add-data-lines!
    data-lines!
    consts/paths-dataline-literal
    paths))


(defn ->remove-signals [paths]
  (u/transient-> []
    (add-remove-signals-paths! paths)))


(comment
  (->remove-signals ["foo.bar" "foo.baz" "bar"])
  := ["paths foo.bar"
      "paths foo.baz"
      "paths bar"])


(defn remove-signals! [sse-gen paths opts]
  (when-not (seq paths)
    (throw (ex-info "Invalid signal paths to remove."
                    {:paths paths})))

  (try
    (sse/send-event! sse-gen
                     consts/event-type-remove-signals
                     (->remove-signals paths)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send remove signals"
                      {:signals paths}
                      e)))))


;; -----------------------------------------------------------------------------
;; Read signals
;; -----------------------------------------------------------------------------
(defn datastar-request? [request]
  (= "true" (get-in request [:headers "datastar-request"])))

 
(defn get-signals
  "Returns the signals json string. You need to use some middleware
  that adds the :query-params key to the request for this function
  to work properly.

  (Bring your own json parsing)"
  [request]
  (if (= :get (:request-method request))
    (get-in request [:query-params consts/datastar-key])
    (:body request)))


