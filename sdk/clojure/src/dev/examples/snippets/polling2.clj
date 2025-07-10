#_{:clj-kondo/ignore true}
(ns examples.snippets.polling2
  (:require
    [dev.onionpancakes.chassis.core :refer [html]]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.common :refer [on-open]]
    [starfederation.datastar.clojure.adapter.test :as at :refer [->sse-response]]))

#_{:clj-kondo/ignore true}
(comment
  (require
    '[starfederation.datastar.clojure.api :as d*]
    '[starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]
    '[some.hiccup.library :refer [html]]))

(import
  'java.time.format.DateTimeFormatter
  'java.time.LocalDateTime)

(def date-time-formatter (DateTimeFormatter/ofPattern "YYYY-MM-DD HH:mm:ss"))
(def seconds-formatter (DateTimeFormatter/ofPattern "ss"))

(defn handler [ring-request]
  (->sse-response ring-request
    {on-open
     (fn [sse]
       (let [now (LocalDateTime/now)
             current-time (LocalDateTime/.format now date-time-formatter)
             seconds (LocalDateTime/.format now seconds-formatter)
             duration (if (neg? (compare seconds "50"))
                        "5"
                        "1")]
         (d*/patch-elements! sse
           (html [:div#time {(str "data-on-interval__duration." duration "s")
                             (d*/sse-get "/endpoint")}
                   current-time]))

         (d*/close-sse! sse)))}))


(comment
  (handler {}))


