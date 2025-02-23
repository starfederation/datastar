(ns starfederation.datastar.clojure.adapter.http-kit-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.adapter.http-kit]
    [starfederation.datastar.clojure.adapter.common-schemas :as cs]))


(m/=> starfederation.datastar.clojure.adapter.http-kit/->sse-response
      [:-> :map cs/->sse-response-options-schema :any])




