(ns starfederation.datastar.clojure.api.scripts-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.api.common-schemas :as cs]
    [starfederation.datastar.clojure.api.scripts]))

(m/=> starfederation.datastar.clojure.api.scripts/->script
      [:-> cs/script-content-schema cs/data-lines-schema])

