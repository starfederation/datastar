(ns examples.malli
  (:require
    [malli.core :as m]
    [malli.instrument :as mi]
    [malli.dev :as mdev]
    [starfederation.datastar.clojure.api.fragments :as f]
    [starfederation.datastar.clojure.api.fragments-schemas]
    [starfederation.datastar.clojure.api.common :as c]))

(comment
  m/-instrument
  (mi/instrument!)
  (mi/unstrument!)
  (mdev/start!)
  (mdev/start! {:exception true})
  (mdev/stop!))

(comment
  (f/->merge-fragment "f" {c/retry-duration :a})
  (f/->merge-fragment "f" {c/retry-duration 1022}))


(comment
  :help
  :dbg
  :rec
  :stop
  :debug)
