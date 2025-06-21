(ns starfederation.datastar.clojure.api.elements-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.api.common-schemas :as cs]
    [starfederation.datastar.clojure.api.elements]))


(m/=> starfederation.datastar.clojure.api.elements/->patch-elements
      [:-> cs/elements-schema cs/patch-element-options-schemas cs/data-lines-schema])


(m/=> starfederation.datastar.clojure.api.elements/->patch-elements-seq
      [:-> cs/elements-seq-schema cs/patch-element-options-schemas cs/data-lines-schema])

