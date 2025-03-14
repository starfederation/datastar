(ns examples.snippets
  (:require
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.common :refer [on-open]]
    [starfederation.datastar.clojure.adapter.test :as at :refer [->sse-response]]))


;; Snippets used in the website docs

(def sse (at/->sse-gen))

;; multiple_events
(d*/merge-fragment! sse "<div id=\"question\">...</div>")
(d*/merge-fragment! sse "<div id=\"instructions\">...</div>")
(d*/merge-signals! sse "{answer: '...'}")
(d*/merge-signals! sse "{prize: '...'}")

;; setup
#_{:clj-kondo/ignore true}
(comment
  (require
    '[starfederation.datastar.clojure.api :as d*]
    '[starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]))


(defn handler [request]
  (->sse-response request
    {on-open
     (fn [sse]
       (d*/merge-fragment! sse
         "<div id=\"question\">What do you put in a toaster?</div>")

       (d*/merge-signals! sse "{response: '', answer: 'bread'}"))}))

(comment
  (handler {}))

 ;; multiple_events going deeper
#_{:clj-kondo/ignore true}
(comment
  (require
    '[starfederation.datastar.clojure.api :as d*]
    '[starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]))


#_{:clj-kondo/ignore true}
(defn handler [request]
  (->sse-response request
    {on-open
      (fn [sse]
        (d*/merge-fragment! sse "<div id=\"hello\">Hello, world!</div>")
        (d*/merge-signals!  sse "{foo: {bar: 1}}")
        (d*/execute-script! sse "console.log('Success!')"))}))
