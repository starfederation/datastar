#_{:clj-kondo/ignore true}
(ns examples.snippets.redirect3
  (:require
    [dev.onionpancakes.chassis.core :refer [html]]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.common :refer [on-open]]
    [starfederation.datastar.clojure.adapter.test :refer [->sse-response]]))


#_{:clj-kondo/ignore true}
(comment
  (require
    '[starfederation.datastar.clojure.api :as d*]
    '[starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]
    '[some.hiccup.library :refer [html]]))


(defn handler [ring-request]
  (->sse-response ring-request
    {on-open
     (fn [sse]
       (d*/patch-elements! sse
         (html [:div#indicator "Redirecting in 3 seconds..."]))
       (Thread/sleep 3000)
       (d*/redirect! sse "/guide")
       (d*/close-sse! sse))}))


(comment
  (handler {}))


