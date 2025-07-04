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
   consts/event-type-patch-elements
   consts/event-type-patch-signals])

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
(def elements-schema :string)
(def elements-seq-schema [:seqable :string])


(def patch-modes-schema
  [:enum
   consts/element-patch-mode-outer
   consts/element-patch-mode-inner
   consts/element-patch-mode-replace
   consts/element-patch-mode-prepend
   consts/element-patch-mode-append
   consts/element-patch-mode-before
   consts/element-patch-mode-after
   consts/element-patch-mode-remove])

(comment
  (m/validate patch-modes-schema consts/element-patch-mode-after)
  (m/validate patch-modes-schema "toto"))


(def patch-element-options-schemas
  (mu/merge
    sse-options-schema
    (mu/optional-keys
      [:map
       [common/selector :string]
       [common/patch-mode patch-modes-schema]
       [common/use-view-transition :boolean]])))

;; -----------------------------------------------------------------------------
(def selector-schema :string)

(def remove-element-options-schemas patch-element-options-schemas)


;; -----------------------------------------------------------------------------
(def signals-schema :string)

(def patch-signals-options-schemas
  (mu/merge
    sse-options-schema
    (mu/optional-keys
      [:map
       [common/only-if-missing :boolean]])))


;; -----------------------------------------------------------------------------
(def signal-paths-schema [:seqable :string])

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


