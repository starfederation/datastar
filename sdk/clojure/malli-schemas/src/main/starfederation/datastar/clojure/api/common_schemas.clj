(ns starfederation.datastar.clojure.api.common-schemas
  (:require
    [malli.core :as m]
    [malli.util :as mu]
    [starfederation.datastar.clojure.api.common :as common]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.protocols :as p]))

(def sse-gen-schema [:fn {:error/message "argument should be a SSEGenerator"}
                     p/sse-gen?])


(def event-type-schema
  [:enum
   consts/event-type-merge-fragments
   consts/event-type-remove-fragments
   consts/event-type-merge-signals
   consts/event-type-remove-signals
   consts/event-type-execute-script])

(def data-lines-schema [:seqable :string])

(def sse-options-schema
  (mu/optional-keys
   [:map
    [common/id :string]
    [common/retry-duration number?]]))


(comment
  (m/validate sse-options-schema {common/id "1"})
  (m/validate sse-options-schema {common/id 1}))

;; -----------------------------------------------------------------------------
(def fragment-schema :string)
(def fragments-schema [:seqable :string])


(def merge-modes-schema
  [:enum
   consts/fragment-merge-mode-morph
   consts/fragment-merge-mode-inner
   consts/fragment-merge-mode-outer
   consts/fragment-merge-mode-prepend
   consts/fragment-merge-mode-append
   consts/fragment-merge-mode-before
   consts/fragment-merge-mode-after
   consts/fragment-merge-mode-upsert-attributes])

(comment
  (m/validate merge-modes-schema consts/fragment-merge-mode-after)
  (m/validate merge-modes-schema "toto"))


(def merge-fragment-options-schemas
  (mu/merge
    sse-options-schema
    (mu/optional-keys
      [:map
       [common/selector :string]
       [common/merge-mode merge-modes-schema]
       [common/settle-duration number?]
       [common/use-view-transition :boolean]])))

;; -----------------------------------------------------------------------------
(def selector-schema :string)

(def remove-fragments-options-schemas
  (mu/merge
    sse-options-schema
    (mu/optional-keys
      [:map
       [common/settle-duration number?]
       [common/use-view-transition :boolean]])))


;; -----------------------------------------------------------------------------
(def signals-schema :string)

(def merge-signals-options-schemas
  (mu/merge
    sse-options-schema
    (mu/optional-keys
      [:map
       [common/only-if-missing :boolean]])))


;; -----------------------------------------------------------------------------
(def signal-paths-schema [:seqable :string])

(def remove-signals-options-schemas sse-options-schema)

;; -----------------------------------------------------------------------------
(def script-content-schema :string)

(def execute-script-options-schemas
  (mu/merge
    sse-options-schema
    (mu/optional-keys
      [:map
       [common/auto-remove :boolean]
       [common/attributes [:map-of [:or :string :keyword] :any]]])))


(comment
  (m/validate execute-script-options-schemas {common/auto-remove true})
  (m/validate execute-script-options-schemas {common/auto-remove "1"})
  (m/validate execute-script-options-schemas {common/attributes {:t1 1}})
  (m/validate execute-script-options-schemas {common/attributes {"t1" 1}})
  (m/validate execute-script-options-schemas {common/attributes {1 1}})
  (m/validate execute-script-options-schemas {common/attributes :t1}))


