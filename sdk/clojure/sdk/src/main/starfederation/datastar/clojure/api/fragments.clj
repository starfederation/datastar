(ns starfederation.datastar.clojure.api.fragments
  (:require
    [clojure.string :as string]
    [starfederation.datastar.clojure.api.common :as common]
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.utils :as u]))



;; -----------------------------------------------------------------------------
;; Selector helpers
(def ^:private valid-selector? u/not-empty-string?)

(defn- add-selector! [data-lines! selector]
  (common/add-opt-line! data-lines!
                        consts/selector-dataline-literal
                        selector))

(defn- add-selector?! [data-lines! selector]
  (common/add-opt-line!
    data-lines!
    valid-selector?
    consts/selector-dataline-literal
    selector))


;; -----------------------------------------------------------------------------
;; Fragment merge mode helpers
(defn- add-fmm? [fmm]
  (and fmm (not= fmm consts/fragment-merge-mode-morph)))


(defn- add-fragment-merge-mode?! [data-lines! fmm]
  (common/add-opt-line!
    data-lines!
    add-fmm?
    consts/merge-mode-dataline-literal
    fmm))


;; -----------------------------------------------------------------------------
;; Settle duration helpers
(defn- add-settle-duration? [d]
  (and d (> d consts/default-fragments-settle-duration)))


(defn- add-settle-duration?! [data-lines! d]
  (common/add-opt-line!
    data-lines!
    add-settle-duration?
    consts/settle-duration-dataline-literal
    d))

;; -----------------------------------------------------------------------------
;; View transition helpers
(defn add-view-transition? [val]
  (common/add-boolean-option? consts/default-fragments-use-view-transitions
                              val))

(defn- add-view-transition?! [data-lines! uvt]
  (common/add-opt-line!
    data-lines!
    add-view-transition?
    consts/use-view-transition-dataline-literal
    uvt))


;; -----------------------------------------------------------------------------
;; Fragment -> data lines
(defn- add-merge-fragment! [data-lines! fragment]
  (cond-> data-lines!
    (u/not-empty-string? fragment)
    (common/add-data-lines! consts/fragments-dataline-literal
                            (string/split-lines fragment))))

;; -----------------------------------------------------------------------------
;; Merge fragment
;; -----------------------------------------------------------------------------
(defn ->merge-fragment [fragment opts]
  (u/transient-> []
    (add-selector?!            (common/selector opts))
    (add-fragment-merge-mode?! (common/merge-mode opts))
    (add-settle-duration?!     (common/settle-duration opts))
    (add-view-transition?!     (common/use-view-transition opts))
    (add-merge-fragment!       fragment)))


(comment
  (->merge-fragment "<div>hello</div>" {})
  :=  ["fragments <div>hello</div>"]

  (->merge-fragment "<div>hello</div> \n<div>world!!!</div>"
                    {common/selector "#toto"
                     common/merge-mode consts/fragment-merge-mode-after
                     common/settle-duration 500
                     common/use-view-transition :toto})
  := ["selector #toto"
      "mergeMode after"
      "settleDuration 500"
      "useViewTransition true"
      "fragments <div>hello</div> "
      "fragments <div>world!!!</div>"])


(defn merge-fragment! [sse-gen fragment opts]
  (try
    (sse/send-event! sse-gen
                     consts/event-type-merge-fragments
                     (->merge-fragment fragment opts)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send fragment."
                      {:fragment fragment}
                      e)))))

;; -----------------------------------------------------------------------------
;; Merge fragments
;; -----------------------------------------------------------------------------
(defn- add-merge-fragments! [data-lines! fragments]
  (cond-> data-lines!
    (seq fragments)
    (common/add-data-lines! consts/fragments-dataline-literal
                            (eduction
                              (comp (mapcat string/split-lines)
                                    (remove string/blank?))
                              fragments))))


(defn ->merge-fragments [fragments opts]
  (u/transient-> []
    (add-selector?!            (common/selector opts))
    (add-fragment-merge-mode?! (common/merge-mode opts))
    (add-settle-duration?!     (common/settle-duration opts))
    (add-view-transition?!     (common/use-view-transition opts))
    (add-merge-fragments!      fragments)))


(defn merge-fragments! [sse-gen fragments opts]
  (try
    (sse/send-event! sse-gen
                     consts/event-type-merge-fragments
                     (->merge-fragments fragments opts)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send fragment."
                      {:fragments fragments}
                      e)))))


(comment
  (->merge-fragments ["<div>hello</div>" "   " "<div>\nworld\n</div>"] {})
  := ["fragments <div>hello</div>"
      "fragments <div>"
      "fragments world"
      "fragments </div>"]

  (->merge-fragments ["<div>hello</div> \n<div>world!!!</div>" "<div>world!!!</div>"]
                     {common/selector "#toto"
                      common/merge-mode consts/fragment-merge-mode-after
                      common/settle-duration 500
                      common/use-view-transition true})
  := ["selector #toto"
      "mergeMode after"
      "settleDuration 500"
      "useViewTransition true"
      "fragments <div>hello</div> "
      "fragments <div>world!!!</div>"
      "fragments <div>world!!!</div>"])


;; -----------------------------------------------------------------------------
;; Remove fragment
;; -----------------------------------------------------------------------------
(defn ->remove-fragment [selector opts]
  (u/transient-> []
    (add-settle-duration?! (common/settle-duration opts))
    (add-view-transition?! (common/use-view-transition opts))
    (add-selector! selector)))


(comment
  (->remove-fragment "#titi"
                     {common/settle-duration 500
                      common/use-view-transition true})
  := ["selector #titi" "settleDuration 500" "useViewTransition true"])


(defn remove-fragment! [sse-gen selector opts]
  (when-not (valid-selector? selector)
    (throw (ex-info "Invalid selector" {:selector selector})))

  (try
    (sse/send-event! sse-gen
                     consts/event-type-remove-fragments
                     (->remove-fragment selector opts)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send remove."
                      {:selector selector}
                      e)))))
 
