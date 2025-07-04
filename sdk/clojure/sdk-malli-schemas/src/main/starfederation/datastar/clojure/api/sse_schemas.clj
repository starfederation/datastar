(ns starfederation.datastar.clojure.api.sse-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.api.common-schemas :as cs]
    [starfederation.datastar.clojure.api.sse]))


(m/=> starfederation.datastar.clojure.api.sse/send-event!
      [:function
       [:-> cs/sse-gen-schema cs/event-type-schema cs/data-lines-schema :any]
       [:-> cs/sse-gen-schema cs/event-type-schema cs/data-lines-schema cs/sse-options-schema :any]])
