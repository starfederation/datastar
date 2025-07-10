(ns starfederation.datastar.clojure.api-test
  (:require
    [clojure.string :as string]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.adapter.test :as at]
    [lazytest.core :as lt :refer [defdescribe describe it expect specify]]))


(def patch-element-t consts/event-type-patch-elements)
(def patch-signals-t consts/event-type-patch-signals)


;; -----------------------------------------------------------------------------
;; Basic event
;; -----------------------------------------------------------------------------
(defn event-beginning
  "Alternate, simplified implementation of
  [starfederation.datastar.clojure.api.sse/write-event!]"
  [event-type & {id             d*/id
                 retry-duration d*/retry-duration}]
  (cond-> [(format "event: %s" event-type)]
    id
    (conj (format "id: %s" id))

    (and retry-duration
         (> retry-duration 0)
         (not= retry-duration consts/default-sse-retry-duration))
    (conj (format "retry: %s" retry-duration))))


(def event-end ["" ""])


(defn event [event-type data-lines & {:as opts}]
  (string/join \newline
    (concat (event-beginning event-type opts)
            data-lines
            event-end)))


;; -----------------------------------------------------------------------------
;; Testing that basic send-event! options
;; -----------------------------------------------------------------------------
(defn basic-test
  "Testing the handling of SSE options."
  [tested-fn input event-type data-lines]
  (describe tested-fn
    (lt/expect-it "sends minimal fragment"
      (= (tested-fn (at/->sse-gen) input {})
         (event event-type  data-lines)))


    (lt/expect-it "handles ids"
      (= (tested-fn (at/->sse-gen) input {d*/id "1"})
         (event event-type  data-lines {d*/id "1"})))


    (it "handles retry duration"
      (expect
        (= (tested-fn (at/->sse-gen) input {d*/retry-duration 1})
           (event event-type  data-lines {d*/retry-duration 1})))

      (expect
        (= (tested-fn (at/->sse-gen) input {d*/retry-duration 0})
           (event event-type  data-lines {d*/retry-duration 0}))))


    (lt/expect-it "handles both"
      (= (tested-fn (at/->sse-gen) input {d*/id "1" d*/retry-duration 1})
         (event event-type  data-lines {d*/id "1" d*/retry-duration 1})))))


(defdescribe test-common-sse-opts
  (basic-test d*/patch-elements!     "" patch-element-t [])
  (basic-test d*/patch-elements-seq! [] patch-element-t [])
  (basic-test d*/patch-signals!      "" patch-signals-t  []))


(comment
  (require '[lazytest.repl :as ltr])
  (ltr/run-test-var #'test-common-sse-opts))


;; -----------------------------------------------------------------------------
;; Patch elements helpers
;; -----------------------------------------------------------------------------
(defn ->data-line [line-literal val]
  (format "data: %s%s" line-literal val))

(def basic-selector "#id")
(def selector-line
  (->data-line consts/selector-dataline-literal basic-selector))

(def test-merge-mode consts/element-patch-mode-after)
(def patch-mode-line
  (->data-line consts/mode-dataline-literal test-merge-mode))

(def use-view-transition-line
  (->data-line consts/use-view-transition-dataline-literal true))

(def ->element-line (partial ->data-line consts/elements-dataline-literal))

;; -----------------------------------------------------------------------------
;; Patch elements tests cases
;; -----------------------------------------------------------------------------
(defn patch-simple-test
  "No options give a simple event with the patch data-lines"
  [tested-patch-fn input expected-datalines]
  (expect (= (tested-patch-fn (at/->sse-gen) input {})
             (event patch-element-t expected-datalines))))


(defn patch-selector-test
  "We see the selector data-line added."
  [tested-patch-fn input expected-datalines]
  (expect (= (tested-patch-fn (at/->sse-gen) input {d*/selector basic-selector})
             (event patch-element-t (list* selector-line expected-datalines)))))


(defn patch-mode-test
  "We see the patch mode data-line added."
  [tested-patch-fn input expected-datalines]
  (expect (= (tested-patch-fn  (at/->sse-gen) input {d*/patch-mode test-merge-mode})
             (event patch-element-t (list* patch-mode-line expected-datalines)))))


(defn patch-vt-false-test
  "No view transition on false"
  [tested-patch-fn input expected-datalines]
  (expect (= (tested-patch-fn (at/->sse-gen) input {d*/use-view-transition false})
             (event patch-element-t expected-datalines))))

(defn patch-vt-non-bool-test
  "No view transition on non boolean."
  [tested-patch-fn input expected-datalines]
  (expect (= (tested-patch-fn (at/->sse-gen) input {d*/use-view-transition :true})
             (event patch-element-t expected-datalines))))


(defn patch-vt-true-test
  "View transition line is added on true."
  [tested-patch-fn input expected-datalines]
  (expect (= (tested-patch-fn (at/->sse-gen) input {d*/use-view-transition true})
             (event patch-element-t (list* use-view-transition-line expected-datalines)))))


(defn patch-all-options-test
  "All options, we see all additional lines."
  [tested-patch-fn input expected-datalines]
  (expect (= (tested-patch-fn (at/->sse-gen)
                              input
                              {d*/selector basic-selector
                               d*/patch-mode test-merge-mode
                               d*/use-view-transition true})
             (event patch-element-t
                    (list*
                      selector-line
                      patch-mode-line
                      use-view-transition-line
                      expected-datalines)))))


;; -----------------------------------------------------------------------------
;; patch-elements! test definition
;; -----------------------------------------------------------------------------
(def div-element "<div>\n  hello\n</div>")
(def div-data
  [(->element-line "<div>")
   (->element-line "  hello")
   (->element-line "</div>")])



(defdescribe test-patch-elements!
  (describe d*/patch-elements!
    (it "handles no options"
      (patch-simple-test           d*/patch-elements! div-element div-data))
    (it "handles selectors"
      (patch-selector-test         d*/patch-elements! div-element div-data))
    (it "handles patch modes"
      (patch-mode-test             d*/patch-elements! div-element div-data))

    (describe "handles view-transitions"
      (specify "no view transition on false"
        (patch-vt-false-test d*/patch-elements! div-element div-data))
      (specify "no view transition on non boolean value"
        (patch-vt-non-bool-test d*/patch-elements! div-element div-data))
      (specify "view transition on true"
        (patch-vt-true-test d*/patch-elements! div-element div-data)))

    (it "handles all options"
      (patch-all-options-test      d*/patch-elements! div-element div-data))))


(comment
  (ltr/run-test-var #'test-patch-elements!))

;; -----------------------------------------------------------------------------
;; patch-elements-seq! test definition
;; -----------------------------------------------------------------------------
(def multi-elements ["<div>\n  hello\n</div>"
                     "<div>\n  world\n</div>"])

(def multi-data
  [(->element-line "<div>")
   (->element-line "  hello")
   (->element-line "</div>")
   (->element-line "<div>")
   (->element-line "  world")
   (->element-line "</div>")])


(defdescribe test-patch-elements-seq!
  (describe d*/patch-elements-seq!
    (it "handles no options"
      (patch-simple-test           d*/patch-elements-seq! multi-elements multi-data))
    (it "handles selectors"
      (patch-selector-test         d*/patch-elements-seq! multi-elements multi-data))
    (it "handles patch modes"
      (patch-mode-test             d*/patch-elements-seq! multi-elements multi-data))

    (describe "handles view-transitions"
      (specify "no view transition on false"
        (patch-vt-false-test d*/patch-elements-seq! multi-elements multi-data))
      (specify "no view transition on non boolean value"
        (patch-vt-non-bool-test d*/patch-elements-seq! multi-elements multi-data))
      (specify "view transition on true"
        (patch-vt-true-test d*/patch-elements-seq! multi-elements multi-data)))

    (it "handles all options"
      (patch-all-options-test      d*/patch-elements-seq! multi-elements multi-data))))



(comment
  (ltr/run-test-var #'test-patch-elements-seq!))



;; -----------------------------------------------------------------------------
;; remove-element! test definition
;; -----------------------------------------------------------------------------
(def patch-mode-remove-line
  (->data-line consts/mode-dataline-literal consts/element-patch-mode-remove))

(defdescribe test-remove-element!
  (describe d*/remove-element!
    (it "produces a well formed event"
        (expect (= (d*/remove-element! (at/->sse-gen) "#id")
                   (event patch-element-t [selector-line patch-mode-remove-line]))))))

(comment
  (ltr/run-test-var #'test-remove-element!))


;; -----------------------------------------------------------------------------
;; Merge signals
;; -----------------------------------------------------------------------------
(def test-signals-content "{\"a\":1,\"b\":2,\"c\":{\"d\":1}}")

(def patch-signals-lines
  [(->data-line consts/signals-dataline-literal test-signals-content)])


(def only-if-missing-line
  (->data-line consts/only-if-missing-dataline-literal true))

(defdescribe test-patch-signals!
  (describe d*/patch-signals!
    (it "works with no options"
        (expect (= (d*/patch-signals! (at/->sse-gen) test-signals-content {})
                   (event patch-signals-t patch-signals-lines))))

    (it "adds only-if-missing line"
        (expect (= (d*/patch-signals! (at/->sse-gen) test-signals-content {d*/only-if-missing true})
                   (event patch-signals-t
                          (list* only-if-missing-line
                                 patch-signals-lines)))))))

(comment
  (ltr/run-test-var #'test-patch-signals!))

;; -----------------------------------------------------------------------------
;; Execute scripts
;; -----------------------------------------------------------------------------
(def script-content "console.log('hello')")

(defn script-event [script-tag]
  (event patch-element-t
         [(->data-line consts/selector-dataline-literal "body")
          (->data-line consts/mode-dataline-literal consts/element-patch-mode-append)
          (->data-line consts/elements-dataline-literal script-tag)]))

(defdescribe test-execute-script!
  (describe d*/execute-script!
    (specify "auto-remove is the default behavior"
        (expect (= (d*/execute-script! (at/->sse-gen) script-content)
                   (script-event "<script data-effect=\"el.remove()\">console.log('hello')</script>"))))

    (specify "we can disable auto-remove"
      (expect (= (d*/execute-script! (at/->sse-gen)
                                     script-content
                                     {d*/auto-remove false})
                 (script-event "<script>console.log('hello')</script>"))))

    (specify "we can disable auto-remove and add attributes"
      (expect (= (d*/execute-script! (at/->sse-gen)
                                     script-content
                                     {d*/auto-remove false
                                      d*/attributes {:type "module"}})
                 (script-event "<script type=\"module\">console.log('hello')</script>"))))

    (specify "we can add attributes auto-remove is there"
      (expect (= (d*/execute-script! (at/->sse-gen)
                                     script-content
                                     {d*/attributes {:type "module" :data-something 1}})
                 (d*/execute-script! (at/->sse-gen)
                                     script-content
                                     {d*/auto-remove true
                                      d*/attributes {:type "module" :data-something 1}})
                 (script-event "<script type=\"module\" data-something=\"1\" data-effect=\"el.remove()\">console.log('hello')</script>"))))))

(comment
  (ltr/run-test-var #'test-execute-script!))


