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
(defn add-only-if-missing? [v]
  (common/add-boolean-option? consts/default-patch-signals-only-if-missing v))

(defn ->patch-signals [signals opts]
  (let [oim (common/only-if-missing opts)]
    (u/transient-> []
      (cond->
        (and oim (add-only-if-missing? oim))
        (common/add-opt-line! consts/only-if-missing-dataline-literal oim)

        (u/not-empty-string? signals)
        (common/add-data-lines! consts/signals-dataline-literal
                                (string/split-lines signals))))))



(comment
  (= (->patch-signals "{'some': \n 'json'}" {})
     ["signals {'some': "
      "signals  'json'}"]))
 
(defn patch-signals! [sse-gen signals-content opts]
  (try
    (sse/send-event! sse-gen
                     consts/event-type-patch-signals
                     (->patch-signals signals-content opts)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send merge signals"
                      {:signals signals-content}
                      e)))))


;; -----------------------------------------------------------------------------
;; Read signals
;; -----------------------------------------------------------------------------
(defn get-signals
  "Returns the signals json string. You need to use some middleware
  that adds the :query-params key to the request for this function
  to work properly.

  (Bring your own json parsing)"
  [request]
  (if (= :get (:request-method request))
    (get-in request [:query-params consts/datastar-key])
    (:body request)))


