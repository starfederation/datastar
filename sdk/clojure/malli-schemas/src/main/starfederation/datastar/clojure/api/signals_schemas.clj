(ns starfederation.datastar.clojure.api.signals-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.api.common-schemas :as cs]
    [starfederation.datastar.clojure.api.signals]))


(m/=> starfederation.datastar.clojure.api.signals/->merge-signals
      [:-> cs/signals-schema cs/merge-signals-options-schemas cs/data-lines-schema])


(m/=> starfederation.datastar.clojure.api.signals/->remove-signals
      [:-> cs/signal-paths-schema cs/data-lines-schema])
