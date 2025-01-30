(ns starfederation.datastar.clojure.api.common)

;; -----------------------------------------------------------------------------
;; Option names
;; -----------------------------------------------------------------------------

;; SSE Options
(def id                  :d*.sse/id)
(def retry-duration      :d*.sse/retry-duration)

;; Merge fragment opts
(def selector            :d*.fragments/selector)
(def merge-mode          :d*.fragments/merge-mode)
(def settle-duration     :d*.fragments/settle-duration)
(def use-view-transition :d*.fragments/use-view-transition)

;;Signals opts
(def only-if-missing     :d*.signals/only-if-missing)

;; Script opts
(def auto-remove         :d*.scripts/auto-remove)
(def attributes          :d*.scripts/attributes)



;; -----------------------------------------------------------------------------
;; Data lines construction helpers
;; -----------------------------------------------------------------------------
(defn add-opt-line!
  "Add an option `v` line to the transient `data-lines!` vector.

  Args:
  - `data-lines`: a transient vector of data-lines that will be written in a sse
    event
  - `prefix`: The Datastar specific preffix for that line
  - `test`: function applied to `v` if the result is true the data-line is
    added, it is elided otherwise. If not test is provided, the data-line will
    be added.
  - `v`: the value for that line
  "
  ([data-lines! prefix v]
   (conj! data-lines! (str prefix v)))
  ([data-lines! test prefix v]
   (cond-> data-lines!
     (test v) (conj! (str prefix v)))))


(defn add-data-lines!
  "Add several data-lines to the `data-lines!` transient vector."
  [data-lines! prefix lines]
  (reduce
    (fn [acc part]
      (conj! acc (str prefix part)))
    data-lines!
    lines))


(defn add-boolean-option?
  "Utility used to test whether an boolean option should result in a sse event
  data-line. Returns true if `val` a boolean and isn't the `default-val`."
  [default-val val]
  (and
    (boolean? val)
    (not= val default-val)))



