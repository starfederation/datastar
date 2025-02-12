(ns examples.snippets.redirect1
  (:require
    [dev.onionpancakes.chassis.core :refer [html]]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.test :refer [->sse-response]]))


(comment
  (require
    '[starfederation.datastar.clojure.api :as d*]
    '[starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response]]
    '[some.hiccup.library :refer [html]]))


(defn handler [ring-request]
  (->sse-response ring-request
    {:on-open
      (fn [sse]
        (d*/merge-fragment! sse
          (html [:div#indicator "Redirecting in 3 seconds..."]))
        (Thread/sleep 3000)
        (d*/execute-script! sse "window.location = \"/guide\"")
        (d*/close-sse! sse))}))

(comment
  (handler {}))


