(ns starfederation.datastar.clojure.adapter.ring-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.adapter.ring]
    [starfederation.datastar.clojure.adapter.common-schemas :as cs]))


(m/=> starfederation.datastar.clojure.adapter.ring/->sse-response
      [:-> :map cs/->sse-response-options-schema :any])


