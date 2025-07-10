(ns examples.data-dsl
  (:require
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.api :as d*]))

;; Examples of how one might want to build a higher level api
;; on top of the SDK

(def example
  {:event ::patch-elements
   :elements "<div>hello</div>"
   d*/selector "foo"
   d*/patch-mode d*/pm-append
   d*/use-view-transition true})



;; -----------------------------------------------------------------------------
;; Pure version just for data-lines
;; -----------------------------------------------------------------------------
(require '[starfederation.datastar.clojure.api.elements :as elements])


(defn sse-event [e]
  (case (:event e)
    ::patch-elements
    (elements/->patch-elements (:elements e) e)))


(sse-event example)
; ["selector foo"
;  "mergeMode append"
;  "useViewTransition true"
;  "fragments <div>hello</div>"]


;; -----------------------------------------------------------------------------
;; Pure version handling buffer
;; -----------------------------------------------------------------------------
(require '[starfederation.datastar.clojure.api.sse :as sse])

(defn elements->str [e]
  (let [buffer (StringBuilder.)]
    (sse/write-event! buffer
                      consts/event-type-patch-elements
                      (elements/->patch-elements (:fragments e) e)
                      e)
    (str buffer)))


(defn event->str [e]
  (case (:event e)
      ::patch-elements
      (elements->str e)))

(event->str example)
; "event: datastar-merge-fragments\n
; retry: 1000\n
; data: selector foo\n
; data: mergeMode append\n
; data: useViewTransition true\n
; data: fragments <div>hello</div>\n\n\n"

;; -----------------------------------------------------------------------------
;; Side effecting version
;; -----------------------------------------------------------------------------
(require '[starfederation.datastar.clojure.adapter.test :as at])

;; SSE generator that returns the sse event string instead of sending it
(def sse-gen (at/->sse-gen))



(defn sse-event! [sse-gen e]
  (case (:event e)
    ::patch-elements (d*/patch-elements! sse-gen (:fragments e) e)))


(sse-event! sse-gen example)
; "event: datastar-merge-fragments\n
; retry: 1000\n
; data: selector foo\n
; data: mergeMode append\n
; data: useViewTransition true\n
; data: fragments <div>hello</div>\n\n\n"
