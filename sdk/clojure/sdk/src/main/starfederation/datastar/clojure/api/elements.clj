(ns starfederation.datastar.clojure.api.elements
  (:require
    [clojure.string :as string]
    [starfederation.datastar.clojure.api.common :as common]
    [starfederation.datastar.clojure.api.sse :as sse]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.utils :as u]))


;; -----------------------------------------------------------------------------
;; Patch Elements options handling
;; -----------------------------------------------------------------------------
(def ^:private valid-selector? u/not-empty-string?)

(defn- add-epm? [fmm]
  (and fmm (not= fmm consts/default-element-patch-mode)))

(defn- add-view-transition? [v]
  (common/add-boolean-option? consts/default-elements-use-view-transitions v))


(defn conj-patch-element-opts!
  "Conj the optional data-lines to the transient `data-lines` vector.
  vector."
  [data-lines! opts]
  (let [sel (common/selector opts)
        patch-mode (common/patch-mode opts)
        use-vt (common/use-view-transition opts)]

    (cond-> data-lines!
      (and sel (valid-selector? sel))
      (common/add-opt-line! consts/selector-dataline-literal sel)
 
      (and patch-mode (add-epm? patch-mode))
      (common/add-opt-line! consts/mode-dataline-literal patch-mode)

      (and use-vt (add-view-transition? use-vt))
      (common/add-opt-line! consts/use-view-transition-dataline-literal use-vt))))



;; -----------------------------------------------------------------------------
;; Patch Element
;; -----------------------------------------------------------------------------
(defn conj-patch-elements!
  "Adds a the data-lines when patching a string of elements."
  [data-lines! element]
  (cond-> data-lines!
    (u/not-empty-string? element)
    (common/add-data-lines! consts/elements-dataline-literal
                            (string/split-lines element))))


(defn ->patch-elements
  "Make the data-lines for a patch-element operation."
  [element opts]
  (u/transient-> []
    (conj-patch-element-opts! opts)
    (conj-patch-elements! element)))


(defn patch-elements! [sse-gen element opts]
  (try
    (sse/send-event! sse-gen
                     consts/event-type-patch-elements
                     (->patch-elements element opts)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send element."
                      {:element element}
                      e)))))

(comment
  (= (->patch-elements "<div>hello</div>" {})
     ["elements <div>hello</div>"])

  (= (->patch-elements "<div>hello</div> \n<div>world!!!</div>"
                       {common/selector "#toto"
                        common/patch-mode consts/element-patch-mode-after
                        common/use-view-transition true})
     ["selector #toto"
      "mode after"
      "useViewTransition true"
      "elements <div>hello</div> "
      "elements <div>world!!!</div>"]))


;; -----------------------------------------------------------------------------
;; Patch Elements
;; -----------------------------------------------------------------------------
(defn conj-patch-elements-seq
  "Adds a the data-lines when patching a seq of strings elements."
  [data-lines! elements-seq]
  (cond-> data-lines!
    (seq elements-seq)
    (common/add-data-lines! consts/elements-dataline-literal
                            (eduction
                              (comp (mapcat string/split-lines)
                                    (remove string/blank?))
                              elements-seq))))


(defn ->patch-elements-seq
  "Make the data-lines for a patch-elements operation."
  [elements-seq opts]
  (u/transient-> []
    (conj-patch-element-opts! opts)
    (conj-patch-elements-seq elements-seq)))


(defn patch-elements-seq! [sse-gen elements opts]
  (try
    (sse/send-event! sse-gen
                     consts/event-type-patch-elements
                     (->patch-elements-seq elements opts)
                     opts)
    (catch Exception e
      (throw (ex-info "Failed to send fragment."
                      {:elements elements}
                      e)))))


(comment
  (= (->patch-elements-seq ["<div>hello</div>" "   " "<div>\nworld\n</div>"] {})
     ["elements <div>hello</div>"
      "elements <div>"
      "elements world"
      "elements </div>"])

  (= (->patch-elements-seq ["<div>hello</div> \n<div>world!!!</div>" "<div>world!!!</div>"]
                       {common/selector "#toto"
                        common/patch-mode consts/element-patch-mode-after
                        common/use-view-transition true})
     ["selector #toto"
      "mode after"
      "useViewTransition true"
      "elements <div>hello</div> "
      "elements <div>world!!!</div>"
      "elements <div>world!!!</div>"]))


;; -----------------------------------------------------------------------------
;; Remove Element
;; -----------------------------------------------------------------------------
(defn remove-element! [sse-gen selector opts]
  (patch-elements! sse-gen "" (assoc opts
                                     common/selector selector
                                     common/patch-mode consts/element-patch-mode-remove)))


