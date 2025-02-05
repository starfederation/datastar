(ns examples.faulty-event
  (:require
    [clojure.pprint :as pp]
    [examples.utils :as u]
    [reitit.ring :as rr]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.ring :refer [->sse-response]]))


(defn faulty-event
  ([req]
   (->sse-response req
     {:on-open
      (fn [sse]
        (d*/with-open-sse sse
          (try
            (d*/console-log! sse
                             "dummy val"
                             {d*/retry-duration :faulty-value})
            (catch Exception _
              (println "Caught the faulty event when sending in sync mode")))))}))
  ([req respond raise]
   (respond
     (->sse-response req
       {:on-open
        (fn [sse]
          (d*/with-open-sse sse
            (try
              (d*/console-log! sse
                               "dummy val"
                               {d*/retry-duration :faulty-value})
              (catch Exception e
                (raise e)))))}))))


(def routes
  [["/error" {:handler faulty-event}]])


(def router
  (rr/router routes))


(def wrap-print-response
  {:name ::print-reponse
   :wrap (fn [handler]
           (fn
             ([req]
              (let [response (handler req)]
                (pp/pprint response)
                response))
             ([req respond raise]
              (handler req
                       #(respond (do
                                   (pp/pprint %)
                                   %))
                       #(do
                          (println "captured the faulty event with raise in async mode")
                          (raise %))))))})


(def default-handler (rr/create-default-handler))

(def handler
  (rr/ring-handler router
                   default-handler
                   {:middleware [wrap-print-response]}))

;; curl -vv http://localhost:8081/error
(comment
  (u/clear-terminal!)
  (u/reboot-jetty-server! #'handler)
  (u/reboot-jetty-server! #'handler {:async? true})
  (u/reboot-rj9a-server! #'handler)
  (u/reboot-rj9a-server! #'handler {:async? true}))

