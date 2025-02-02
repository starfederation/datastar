(ns starfederation.datastar.clojure.api-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.api]
    [starfederation.datastar.clojure.api.common-schemas :as cs]))


(m/=> starfederation.datastar.clojure.api/close-sse!
      [:-> cs/sse-gen-schema :any])


(m/=> starfederation.datastar.clojure.api/merge-fragment!
      [:function
       [:-> cs/sse-gen-schema cs/fragment-schema :any]
       [:-> cs/sse-gen-schema cs/fragment-schema cs/merge-fragment-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/merge-fragments!
      [:function
       [:-> cs/sse-gen-schema cs/fragments-schema :any]
       [:-> cs/sse-gen-schema cs/fragments-schema cs/merge-fragment-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/remove-fragment!
      [:function
       [:-> cs/sse-gen-schema cs/selector-schema :any]
       [:-> cs/sse-gen-schema cs/selector-schema cs/remove-fragments-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/merge-signals!
      [:function
       [:-> cs/sse-gen-schema cs/signals-schema :any]
       [:-> cs/sse-gen-schema cs/signals-schema cs/merge-signals-options-schemas :any]])


(m/=> starfederation.datastar.clojure.api/remove-signals!
      [:function
       [:-> cs/sse-gen-schema cs/signal-paths-schema :any]
       [:-> cs/sse-gen-schema cs/signal-paths-schema cs/remove-signals-options-schemas :any]])


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


