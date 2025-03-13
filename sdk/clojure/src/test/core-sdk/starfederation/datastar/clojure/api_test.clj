(ns starfederation.datastar.clojure.api-test
  (:require
    [clojure.string :as string]
    [clojure.template :as ct]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.adapter.test :as at]
    [lazytest.core :as lt :refer [defdescribe describe it expect]]))


(def merge-fragment-t consts/event-type-merge-fragments)
(def remove-fragment-t consts/event-type-remove-fragments)
(def merge-signals-t consts/event-type-merge-signals)
(def remove-signals-t consts/event-type-remove-signals)
(def execute-script-t consts/event-type-execute-script)


(defn ->data-line [line-literal val]
  (format "data: %s%s" line-literal val))


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


(def event-end ["" "" ""])


(defn event [event-type data-lines & {:as opts}]
  (string/join \newline
    (concat (event-beginning event-type opts)
            data-lines
            event-end)))


(def basic-selector "#id")
(def sel-id-data-line "data: selector #id")


;; -----------------------------------------------------------------------------
;; Testing that basic send-event! options
;; -----------------------------------------------------------------------------
(defmacro basic-test
  "Testing management of sse level options by high level functions."
  [sse-gen tested-fn input event-type data-lines]
  (ct/apply-template
   '[sse-gen tested-fn input event-type data-lines]
   '(describe tested-fn
      (lt/expect-it "sends minimal fragment"
        (= (tested-fn sse-gen input     {})
           (event event-type  data-lines)))


      (lt/expect-it "handles ids"
        (= (tested-fn sse-gen input      {d*/id "1"})
           (event event-type  data-lines {d*/id "1"})))


      (it "handles retry duration"
        (expect
          (= (tested-fn sse-gen input      {d*/retry-duration 1})
             (event event-type  data-lines {d*/retry-duration 1})))

        (expect
          (= (tested-fn sse-gen input      {d*/retry-duration 0})
             (event event-type  data-lines {d*/retry-duration 0}))))


      (lt/expect-it "handles both"
        (= (tested-fn sse-gen input      {d*/id "1" d*/retry-duration 1})
           (event event-type  data-lines {d*/id "1" d*/retry-duration 1}))))
   [sse-gen tested-fn input event-type data-lines]))


(defdescribe common-opts
  (let [sse-gen (at/->sse-gen)
        remove-fragment-expected-dls [sel-id-data-line]]
    (basic-test sse-gen d*/merge-fragment!  ""             merge-fragment-t  [])
    (basic-test sse-gen d*/merge-fragments! []             merge-fragment-t  [])
    (basic-test sse-gen d*/remove-fragment! basic-selector remove-fragment-t remove-fragment-expected-dls)
    (basic-test sse-gen d*/merge-signals!   ""             merge-signals-t   [])
    (basic-test sse-gen d*/execute-script!  ""             execute-script-t  [])))
 

;; -----------------------------------------------------------------------------
;; Merge fragment
;; -----------------------------------------------------------------------------
(def div-fragment "<div>\n  hello\n</div>")

(def ->fragment-line (partial ->data-line consts/fragments-dataline-literal))

(def div-data
  [(->fragment-line "<div>")
   (->fragment-line "  hello")
   (->fragment-line "</div>")])

(def selector-line
  (->data-line consts/selector-dataline-literal basic-selector))

(def test-merge-mode consts/fragment-merge-mode-after)
(def merge-mode-line
  (->data-line consts/merge-mode-dataline-literal test-merge-mode))

(def test-settle-duration 500)
(def settle-duration-line
  (->data-line consts/settle-duration-dataline-literal test-settle-duration))


(def use-view-transition-line
  (->data-line consts/use-view-transition-dataline-literal true))


(defmacro basic-expect [sse-gen tested-fn input opts event-type data-lines]
  (ct/apply-template
    '[sse-gen tested-fn input opts event-type data-lines]
    '(expect (= (tested-fn sse-gen input opts)
                (event event-type data-lines)))
    [sse-gen tested-fn input opts event-type data-lines]))


(defmacro expect-merge-fragment [sse-gen opts data-lines]
  (ct/apply-template
    '[sse-gen opts data-lines]
    '(basic-expect sse-gen
                   d*/merge-fragment! div-fragment opts
                   merge-fragment-t data-lines)
    [sse-gen opts data-lines]))


(defdescribe merge-fragment!
  (let [gen (at/->sse-gen)]
    (describe d*/merge-fragment!

      (it "works for a basic fragment"
        (expect-merge-fragment gen {} div-data))

      (it "handles selectors"
        (expect-merge-fragment gen {d*/selector basic-selector}
                                   (list* selector-line div-data)))

      (it "handles merge-mode"
        (expect-merge-fragment gen {d*/merge-mode test-merge-mode}
                                   (list* merge-mode-line div-data)))

      (it "handles settle duration"
        (expect-merge-fragment gen {d*/settle-duration test-settle-duration}
                                   (list* settle-duration-line div-data))
        (expect-merge-fragment gen {d*/settle-duration consts/default-fragments-settle-duration}
                                   div-data))

 
      (it "handles view transitions"
        (expect-merge-fragment gen {d*/use-view-transition true}
                                   (list* use-view-transition-line div-data))
        (expect-merge-fragment gen {d*/use-view-transition :true} div-data)
        (expect-merge-fragment gen {d*/use-view-transition false} div-data))
 
      (it "handles all options"
        (expect-merge-fragment gen {d*/selector basic-selector
                                    d*/merge-mode test-merge-mode
                                    d*/settle-duration test-settle-duration
                                    d*/use-view-transition true}
                                   (list* selector-line
                                          merge-mode-line
                                          settle-duration-line
                                          use-view-transition-line
                                          div-data))))))


;; -----------------------------------------------------------------------------
;; Merge fragments
;; -----------------------------------------------------------------------------
(def multi-fragments ["<div>\n  hello\n</div>"
                      "<div>\n  world\n</div>"])

(def multi-data
  [(->fragment-line "<div>")
   (->fragment-line "  hello")
   (->fragment-line "</div>")
   (->fragment-line "<div>")
   (->fragment-line "  world")
   (->fragment-line "</div>")])



(defmacro expect-merge-fragments [sse-gen opts data-lines]
  (ct/apply-template
    '[sse-gen opts data-lines]
    '(basic-expect sse-gen
                   d*/merge-fragments! multi-fragments opts
                   merge-fragment-t data-lines)
    [sse-gen opts data-lines]))


(defdescribe merge-fragments!
  (let [gen (at/->sse-gen)]
    (describe d*/merge-fragment!

      (it "works for a basic fragment"
        (expect-merge-fragments gen {} multi-data))

      (it "handles selectors"
        (expect-merge-fragments gen {d*/selector basic-selector}
                                    (list* selector-line multi-data)))

      (it "handles merge-mode"
        (expect-merge-fragments gen {d*/merge-mode test-merge-mode}
                                    (list* merge-mode-line multi-data)))

      (it "handles settle duration"
        (expect-merge-fragments gen {d*/settle-duration test-settle-duration}
                                    (list* settle-duration-line multi-data))
        (expect-merge-fragments gen {d*/settle-duration consts/default-fragments-settle-duration}
                                    multi-data))

 
      (it "handles view transitions"
        (expect-merge-fragments gen {d*/use-view-transition true}
                                    (list* use-view-transition-line multi-data))
        (expect-merge-fragments gen {d*/use-view-transition :true}
                                multi-data)
        (expect-merge-fragments gen {d*/use-view-transition false}
                                multi-data))
 
      (it "handles all options"
        (expect-merge-fragments gen {d*/selector basic-selector
                                     d*/merge-mode test-merge-mode
                                     d*/settle-duration test-settle-duration
                                     d*/use-view-transition true}
                                    (list* selector-line
                                           merge-mode-line
                                           settle-duration-line
                                           use-view-transition-line
                                           multi-data))))))


;; -----------------------------------------------------------------------------
;; Remove fragments
;; -----------------------------------------------------------------------------
(defmacro expect-remove-fragment [sse-gen opts data-lines]
  (ct/apply-template
    '[sse-gen opts data-lines]
    '(basic-expect sse-gen
                   d*/remove-fragment! basic-selector opts
                   remove-fragment-t data-lines)
    [sse-gen opts data-lines]))


(defdescribe remove-fragment!
  (let [gen (at/->sse-gen)]
    (describe d*/remove-fragment!
      (it "Throws on no selector"
        (expect (lt/throws? clojure.lang.ExceptionInfo #(d*/remove-fragment! gen "" {}))))

      (it "handles settle duration"
        (expect-remove-fragment gen {d*/settle-duration test-settle-duration}
                                    [settle-duration-line selector-line])
        (expect-remove-fragment gen {d*/settle-duration consts/default-fragments-settle-duration}
                                    [selector-line]))


      (it "handles view transitions"
        (expect-remove-fragment gen {d*/use-view-transition true}
                                [use-view-transition-line selector-line])
        (expect-remove-fragment gen {d*/use-view-transition :true}
                                [selector-line])
        (expect-remove-fragment gen {d*/use-view-transition false}
                                    [selector-line]))

      (it "handles all options"
        (expect-remove-fragment gen {d*/settle-duration test-settle-duration
                                     d*/use-view-transition true}
                                    [settle-duration-line
                                     use-view-transition-line
                                     selector-line])))))

;; -----------------------------------------------------------------------------
;; Merge signals
;; -----------------------------------------------------------------------------

(def test-signals-content-1 "{\"a\":1,\"b\":2,\"c\":{\"d\":1}}")
(def test-signals-content-2 "{\"toto\":\"a\"}")

(def test-signals-content
  (string/join \newline [test-signals-content-1 test-signals-content-2]))

(def ->merge-signals-line (partial ->data-line consts/signals-dataline-literal))

(def test-signals-lines
  [(->merge-signals-line test-signals-content-1)
   (->merge-signals-line test-signals-content-2)])


(def only-if-missing-line
  (->data-line consts/only-if-missing-dataline-literal true))


(defmacro expect-merge-signals [sse-gen opts data-lines]
  (ct/apply-template
    '[sse-gen opts data-lines]
    '(basic-expect sse-gen
                   d*/merge-signals! test-signals-content opts
                   merge-signals-t data-lines)
    [sse-gen opts data-lines]))


(defdescribe merge-signals
  (let [gen (at/->sse-gen)]
    (describe d*/merge-fragment!
 
      (it "works for a basic signal"
        (expect-merge-signals gen {} test-signals-lines))


      (it "handles the only if missing option"
        (expect-merge-signals gen {d*/only-if-missing true}
                              (list* only-if-missing-line test-signals-lines))
        (expect-merge-signals gen {d*/only-if-missing :true}
                              test-signals-lines)
        (expect-merge-signals gen {d*/only-if-missing false}
                                  test-signals-lines)))))


;; -----------------------------------------------------------------------------
;; Remove signals
;; -----------------------------------------------------------------------------
(def test-signal-paths ["foo.bar" "foo.baz" "bar"])

(def ->remove-signals-line (partial ->data-line consts/paths-dataline-literal))

(def remove-signals-lines
  [(->remove-signals-line "foo.bar")
   (->remove-signals-line "foo.baz")
   (->remove-signals-line "bar")])


(defdescribe remove-signals!
  (let [gen (at/->sse-gen)]
    (describe d*/remove-signals!
      (it "Throws on no paths"
        (expect (lt/throws? clojure.lang.ExceptionInfo #(d*/remove-signals! gen []))))

      (it "works for several paths"
        (expect
          (= (d*/remove-signals! gen test-signal-paths)
             (event remove-signals-t remove-signals-lines)))))))


;; -----------------------------------------------------------------------------
;; Execute scripts
;; -----------------------------------------------------------------------------
(def script-content
  "console.log('hello')\nconsole.log('world!!!')")

(def ->script-line (partial ->data-line consts/script-dataline-literal))

(def script-content-lines
  [(->script-line "console.log('hello')")
   (->script-line "console.log('world!!!')")])

(def test-attrs {:attr1 "val1" :attr2 "val2"})

(def ->attr-line (partial ->data-line consts/attributes-dataline-literal))

(def test-attrs-lines
  [(->attr-line "attr1 val1")
   (->attr-line "attr2 val2")])

(def auto-remove-line
  (->data-line consts/auto-remove-dataline-literal false))


(defmacro expect-execute-script [sse-gen opts data-lines]
  (ct/apply-template
    '[sse-gen opts data-lines]
    '(basic-expect sse-gen
                   d*/execute-script! script-content opts
                   execute-script-t data-lines)
    [sse-gen opts data-lines]))


(defdescribe execute-script
  (let [gen (at/->sse-gen)]
    (describe d*/execute-script!

      (it "works for a basic script"
        (expect-execute-script gen {} script-content-lines))

      (describe "the handling script attributes"
        (it "doesn't add the default type attribute"
          (expect-execute-script gen {d*/attributes {:type "module"}}
                                     script-content-lines))

        (it "adds attributes"
          (expect-execute-script gen {d*/attributes test-attrs}
                                     (concat test-attrs-lines
                                             script-content-lines))))

      (describe "handles the auto remove opt"
        (it "auto removes on truthy values"
          (expect-execute-script gen {d*/auto-remove :true} script-content-lines))

        (it "auto remove on false value"
          (expect-execute-script gen {d*/auto-remove nil}   script-content-lines)
          (expect-execute-script gen {d*/auto-remove false} (list* auto-remove-line script-content-lines))))

      (it "handles all options at once"
        (expect-execute-script gen {d*/attributes test-attrs
                                    d*/auto-remove true}
                                   (concat test-attrs-lines
                                           script-content-lines))

        (expect-execute-script gen {d*/attributes test-attrs
                                    d*/auto-remove false}
                                   (concat test-attrs-lines
                                           [auto-remove-line]
                                           script-content-lines))))))

