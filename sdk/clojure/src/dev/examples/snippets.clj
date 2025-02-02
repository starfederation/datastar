(ns examples.snippets
  (:require
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.test :as at]))



(def sse (at/->sse-gen))

;; multiple_events
(d*/merge-fragment! sse "<div id=\"question\">...</div>")
(d*/merge-fragment! sse "<div id=\"instructions\">...</div>")
(d*/merge-signals! sse "{answer: '...'}")
(d*/merge-signals! sse "{prize: '...'}")

;; setup
(comment
  (require
    '[starfederation.datastar.clojure.api :as d*]
    '[starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response]])


  (defn handler [request]
    (->sse-response request)
    {:on-open
      (fn [sse]
        (d*/merge-fragment! sse
          "<div id=\"question\">What do you put in a toaster?</div>")

        (d*/merge-signals! sse "{response: '', answer: 'bread'}"))})


  ;; multiple_events going deeper
  (require
    '[starfederation.datastar.clojure.api :as d*]
    '[starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response]])


  (defn handler [request]
    (->sse-response request)
    {:on-open
      (fn [sse]
        (d*/merge-fragment! sse "<div id=\"hello\">Hello, world!</div>")
        (d*/merge-signals!  sse "{foo: {bar: 1}}")
        (d*/execute-script! sse "console.log('Success!')"))}))
