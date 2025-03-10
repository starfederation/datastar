(ns starfederation.datastar.clojure.adapter.http-kit-schemas
  (:require
    [malli.core :as m]
    [malli.util :as mu]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.adapter.http-kit]
    [starfederation.datastar.clojure.adapter.common-schemas :as cs]))


(def options-schema
    (mu/update-in cs/->sse-response-options-schema
                  [ac/write-profile]
                  (fn [x]
                    (mu/optional-keys x [ac/wrap-output-stream]))))


(m/=> starfederation.datastar.clojure.adapter.http-kit/->sse-response
      [:-> :map options-schema :any])




