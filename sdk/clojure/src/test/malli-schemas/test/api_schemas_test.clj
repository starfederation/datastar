(ns test.api-schemas-test
  (:require
    [lazytest.core :as lt :refer [defdescribe describe expect it]]
    [malli.instrument :as mi]
    [starfederation.datastar.clojure.adapter.test :as at]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.api-schemas]
    [starfederation.datastar.clojure.api.fragments :as frags])
  (:import
    clojure.lang.ExceptionInfo))


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
#_{:clj-kondo/ignore true}
(def thunk-wrong-script-type #(d*/execute-script! sse-gen :test))
(def thunk-wrong-option-type #(d*/execute-script! sse-gen dumy-script {d*/auto-remove :test}))


(defdescribe malli-schemas
  (describe "without malli"
    (it "error can go through"
      (expect (lt/throws? ExceptionInfo thunk-wrong-script-type))
      (expect (= (d*/execute-script! sse-gen dumy-script {d*/auto-remove :wrong-type})
                 "event: datastar-execute-script\ndata: script console.log('hello')\n\n\n"))))

  (describe "with malli"
    {:context [with-malli]}
    (it "types are checked"
      (let [msg1 (get-exception-msg thunk-wrong-script-type)
            msg2 (get-exception-msg thunk-wrong-option-type)]
       (expect (= msg1 malli-error-msg))
       (expect (= msg2 malli-error-msg)))))

  (describe "Schemas not required"
    (it "doesn't trigger instrumentation"
      (expect (= (frags/->merge-fragment "" {d*/retry-duration :test})
                 [])))))







