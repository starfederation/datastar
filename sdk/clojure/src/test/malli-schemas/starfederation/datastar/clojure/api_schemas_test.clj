(ns starfederation.datastar.clojure.api-schemas-test
  (:require
    [lazytest.core :as lt :refer [defdescribe describe expect it]]
    [malli.instrument :as mi]
    [starfederation.datastar.clojure.adapter.test :as at]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.api.elements :as elements]
    [starfederation.datastar.clojure.api-schemas]))


(def with-malli
  (lt/around [f]
    (mi/instrument!)
    (f)
    (mi/unstrument!)))


(def sse-gen (at/->sse-gen))


(defn get-exception [thunk]
  (try
    (thunk)
    nil
    (catch Exception e
      e)))


(defn get-exception-msg [thunk]
  (-> thunk
      (get-exception)
      ex-message))


(def malli-error-msg
  ":malli.core/invalid-input")


(def dumy-script "console.log('hello')")

(def thunk-wrong-script-type #(d*/execute-script! sse-gen :test))
(def thunk-wrong-option-type #(d*/execute-script! sse-gen dumy-script {d*/auto-remove :test}))


(defdescribe test-malli-schemas
  (describe "without malli"
    (it "error can go through"
      (expect (= (thunk-wrong-script-type)
                 "event: datastar-patch-elements\ndata: selector body\ndata: mode append\ndata: elements <script data-effect=\"el.remove()\">:test</script>\n\n"))
      (expect (= (thunk-wrong-option-type)
                 "event: datastar-patch-elements\ndata: selector body\ndata: mode append\ndata: elements <script data-effect=\"el.remove()\">console.log('hello')</script>\n\n"))))

  (describe "with malli"
    {:context [with-malli]}
    (it "types are checked"
      (let [msg1 (get-exception-msg thunk-wrong-script-type)
            msg2 (get-exception-msg thunk-wrong-option-type)]
       (expect (= msg1 malli-error-msg))
       (expect (= msg2 malli-error-msg)))))

  (describe "Schemas not required"
    (it "doesn't trigger instrumentation"
      (expect (= (elements/->patch-elements "" {d*/retry-duration :test})
                 [])))))

(comment
  (require '[lazytest.repl :as ltr])
  (ltr/run-test-var #'test-malli-schemas))

