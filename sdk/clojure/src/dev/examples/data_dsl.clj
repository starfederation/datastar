(ns examples.data-dsl
  (:require
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.api :as d*]))

(def example
  {:event ::merge-fragments
   :fragments ["<div>hello</div>"]
   d*/selector "foo"
   d*/merge-mode d*/mm-append
   d*/settle-duration 500
   d*/use-view-transition true})



;; -----------------------------------------------------------------------------
;; Pure version just for data-lines
;; -----------------------------------------------------------------------------
(require '[starfederation.datastar.clojure.api.fragments :as frags])


(defn sse-event [e]
  (case (:event e)
    ::merge-fragments
    (frags/->merge-fragments (:fragments e) e)))


(sse-event example)
; ["selector foo"
;  "mergeMode append"
;  "settleDuration 500"
;  "useViewTransition true"
;  "fragments <div>hello</div>"]


;; -----------------------------------------------------------------------------
;; Pure version handling buffer
;; -----------------------------------------------------------------------------
(require '[starfederation.datastar.clojure.api.sse :as sse])

(defn fragment->str [e]
  (let [buffer (StringBuilder.)]
    (sse/write-event! buffer
                      consts/event-type-merge-fragments
                      (frags/->merge-fragments (:fragments e) e)
                      e)
    (str buffer)))


(defn event->str [e]
  (case (:event e)
      ::merge-fragments
      (fragment->str e)))

(event->str example)
; "event: datastar-merge-fragments\n
; retry: 1000\n
; data: selector foo\n
; data: mergeMode append\n
; data: settleDuration 500\n
; data: useViewTransition true\n
; data: fragments <div>hello</div>\n\n\n"

;; -----------------------------------------------------------------------------
;; Side effecting version
;; -----------------------------------------------------------------------------
(require '[starfederation.datastar.clojure.adapters.test :as at])

;; SSE generator that returns the sse event string instead of sending it
(def sse-gen (at/->sse-gen))



(defn sse-event! [sse-gen e]
  (case (:event e)
    ::merge-fragments (d*/merge-fragments! sse-gen (:fragments e) e)))


(sse-event! sse-gen example)
; "event: datastar-merge-fragments\n
; retry: 1000\n
; data: selector foo\n
; data: mergeMode append\n
; data: settleDuration 500\n
; data: useViewTransition true\n
; data: fragments <div>hello</div>\n\n\n"
