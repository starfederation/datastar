(ns starfederation.datastar.clojure.api-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.api]
    [starfederation.datastar.clojure.api.common-schemas :as cs]))


(m/=> starfederation.datastar.clojure.api/close-sse!
      [:-> cs/sse-gen-schema :any])


(m/=> starfederation.datastar.clojure.api/patch-elements!
      [:function
       [:-> cs/sse-gen-schema cs/elements-schema :any]
       [:-> cs/sse-gen-schema cs/elements-schema cs/patch-element-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/patch-elements-seq!
      [:function
       [:-> cs/sse-gen-schema cs/elements-seq-schema :any]
       [:-> cs/sse-gen-schema cs/elements-seq-schema cs/patch-element-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/remove-element!
      [:function
       [:-> cs/sse-gen-schema cs/selector-schema :any]
       [:-> cs/sse-gen-schema cs/selector-schema cs/remove-element-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/patch-signals!
      [:function
       [:-> cs/sse-gen-schema cs/signals-schema :any]
       [:-> cs/sse-gen-schema cs/signals-schema cs/patch-signals-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/execute-script!
      [:function
       [:-> cs/sse-gen-schema cs/script-content-schema :any]
       [:-> cs/sse-gen-schema cs/script-content-schema cs/execute-script-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/sse-get
      [:function
       [:-> :string :string]
       [:-> :string :string :string]])

(m/=> starfederation.datastar.clojure.api/sse-post
      [:function
       [:-> :string :string]
       [:-> :string :string :string]])


(m/=> starfederation.datastar.clojure.api/sse-put
      [:function
       [:-> :string :string]
       [:-> :string :string :string]])

(m/=> starfederation.datastar.clojure.api/sse-patch
      [:function
       [:-> :string :string]
       [:-> :string :string :string]])

(m/=> starfederation.datastar.clojure.api/sse-delete
      [:function
       [:-> :string :string]
       [:-> :string :string :string]])


(m/=> starfederation.datastar.clojure.api/console-log!
      [:function
       [:-> cs/sse-gen-schema :string :any]
       [:-> cs/sse-gen-schema :string cs/execute-script-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/console-error!
      [:function
       [:-> cs/sse-gen-schema :string :any]
       [:-> cs/sse-gen-schema :string cs/execute-script-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/redirect!
      [:function
       [:-> cs/sse-gen-schema :string :any]
       [:-> cs/sse-gen-schema :string cs/execute-script-options-schemas :any]])


