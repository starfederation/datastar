(ns starfederation.datastar.clojure.api.signals-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.api.common-schemas :as cs]
    [starfederation.datastar.clojure.api.signals]))


(m/=> starfederation.datastar.clojure.api.signals/->patch-signals
      [:-> cs/signals-schema cs/patch-signals-options-schemas cs/data-lines-schema])

