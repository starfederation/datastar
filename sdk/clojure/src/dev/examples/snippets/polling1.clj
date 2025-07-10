#_{:clj-kondo/ignore true}
(ns examples.snippets.polling1
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

(import
  'java.time.format.DateTimeFormatter
  'java.time.LocalDateTime)

(def formatter (DateTimeFormatter/ofPattern "YYYY-MM-DD HH:mm:ss"))

(defn handler [ring-request]
  (->sse-response ring-request
    {on-open
     (fn [sse]
       (d*/patch-elements! sse
         (html [:div#time {:data-on-interval__duration.5s (d*/sse-get "/endpoint")}
                 (LocalDateTime/.format (LocalDateTime/now) formatter)]))
       (d*/close-sse! sse))}))

(comment
  (handler {}))


