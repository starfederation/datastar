(ns starfederation.datastar.clojure.api.fragments-schemas
  (:require
    [malli.core :as m]
    [starfederation.datastar.clojure.api.common-schemas :as cs]
    [starfederation.datastar.clojure.api.fragments]))


(m/=> starfederation.datastar.clojure.api.fragments/->merge-fragment
      [:-> cs/fragment-schema cs/merge-fragment-options-schemas cs/data-lines-schema])


(m/=> starfederation.datastar.clojure.api.fragments/->merge-fragments
      [:-> cs/fragments-schema cs/merge-fragment-options-schemas cs/data-lines-schema])


(m/=> starfederation.datastar.clojure.api.fragments/->remove-fragment
      [:-> cs/selector-schema cs/remove-fragments-options-schemas cs/data-lines-schema])
