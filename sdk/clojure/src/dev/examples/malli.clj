(ns examples.malli
  (:require
    [malli.core :as m]
    [malli.instrument :as mi]
    [malli.dev :as mdev]
    [starfederation.datastar.clojure.adapter.test :as at]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.api.fragments :as f]
    [starfederation.datastar.clojure.api.fragments-schemas]
    [starfederation.datastar.clojure.api.common :as c]))

;; Testing how instrumentation works and how it's activated
(comment
  m/-instrument
  (mi/instrument!)
  (mi/unstrument!)
  (mdev/start!)
  (mdev/start! {:exception true})
  (mdev/stop!))

(comment
  (d*/merge-fragment! {} "frag")
  (d*/merge-fragment! (at/->sse-gen) "frag")
  (f/->merge-fragment "f" {c/retry-duration :a})
  (f/->merge-fragment "f" {c/retry-duration 1022}))


(comment
  #_{:clj-kondo/ignore true}
  (user/reload!)
  :help
  :dbg
  :rec
  :stop
  :debug)
