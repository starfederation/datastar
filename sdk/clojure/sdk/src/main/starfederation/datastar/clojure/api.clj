(ns starfederation.datastar.clojure.api
  "
Public api for the Datastar SDK.

The main api consists several functions that operate on SSE generators, see:
- [[patch-elements!]]
- [[patch-elements-seq!]]
- [[remove-element!]]
- [[patch-signals!]]
- [[execute-script!]]

These function take options map whose keys are:
- [[id]]
- [[retry-duration]]
- [[selector]]
- [[patch-mode]]
- [[use-view-transition]]
- [[only-if-missing]]
- [[auto-remove]]
- [[attributes]]

To help manage SSE generators's underlying connection there is:
- [[close-sse!]]
- [[with-open-sse]]

Some common utilities for HTTP are also provided:
- [[sse-get]]
- [[sse-post]]
- [[sse-put]]
- [[sse-patch]]
- [[sse-delete]]

Some scripts are provided:
- [[console-log!]]
- [[console-error!]]
- [[redirect!]]"
  (:require
    [starfederation.datastar.clojure.api.common   :as common]
    [starfederation.datastar.clojure.api.elements :as elements]
    [starfederation.datastar.clojure.api.signals  :as signals]
    [starfederation.datastar.clojure.api.scripts  :as scripts]
    [starfederation.datastar.clojure.consts       :as consts]
    [starfederation.datastar.clojure.protocols    :as p]
    [starfederation.datastar.clojure.utils        :as u]))

;; -----------------------------------------------------------------------------
;; SSE generator management
;; -----------------------------------------------------------------------------
(defmacro lock-sse!
  "Hold onto the lock of a `sse-gen` while executing `body`. This allows for
  preventing concurent sending of sse events. Sse generators use
  [[java.util.concurrent.locks.ReentrantLock]] under the hood.

  Ex:
  ```clojure
  (lock-sse! my-sse-gen
             (patch-elements! sse frags)
             (patch-signals!  sse signals))
  ```
  "
  [sse-gen & body]
  `(u/lock! (p/get-lock ~sse-gen) ~@body))


(comment
  (macroexpand-1
    (macroexpand-1
      '(lock-sse! my-sse-gen
                  (patch-elements! sse frags)
                  (patch-signals!  sse signals)))))


(defn close-sse!
  "Close the connection of a sse generator.

  Return value:
  - true if `sse-gen` closed
  - false if it was already closed"
  [sse-gen]
  (p/close-sse! sse-gen))


(defmacro with-open-sse
  "Macro functioning similarly to [[clojure.core/with-open]]. It evalutes the
  `body` inside a try expression and closes the `sse-gen` at the end using
  [[close-see!]] in a finally clause.

  Ex:
  ```
  (with-open-sse sse-gen
    (d*/patch-elements! sse-gen frag1)
    (d*/patch-signals!  sse-gen signals))
  ```
  "
  [sse-gen & body]
  `(try
     ~@body
     (finally
       (close-sse! ~sse-gen))))

(comment
  (macroexpand-1
    '(with-open-sse toto
       (do-stuff)
       (do-stuff))))


;; -----------------------------------------------------------------------------
;; Option names
;; -----------------------------------------------------------------------------
(def id
  "SSE option use in all event functions, string:

  Each event may include an eventId. This can be used by
  the backend to replay events. This is part of the SSE spec and is used to
  tell the browser how to handle the event. For more details see
  https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id"
  common/id)

(def retry-duration
  "SSE option used in all event functions, number:

  Each event may include a retryDuration value. If one is
  not provided the SDK must default to 1000 milliseconds. This is part of the
  SSE spec and is used to tell the browser how long to wait before reconnecting
  if the connection is lost. For more details see
  https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry"
  common/retry-duration)

;; patch element opts
(def selector
  "[[patch-elements!]] & [[patch-elements-seq!]] option, string:

  The CSS selector to use to insert the elements. If not
  provided or empty, Datastar will default to using the id attribute of the
  element."
  common/selector)

(def patch-mode
  "[[patch-elements!]] & [[patch-elements-seq!]] option, string:

  The mode to use when merging elements into the DOM.
  If not provided the Datastar client side will default to morph.

  The set of valid values is:
  - [[pm-outer]] default
  - [[pm-inner]]
  - [[pm-remove]]
  - [[pm-prepend]]
  - [[pm-append]]
  - [[pm-before]]
  - [[pm-after]]
  - [[pm-replace]]
  "
  common/patch-mode)

(def use-view-transition
  "[[patch-elements!]] / [[remove-element!]  option, boolean:

  Whether to use view transitions, if not provided the
  Datastar client side will default to false."
  common/use-view-transition)

;;Signals opts
(def only-if-missing
  "[[patch-signals!]] option, boolean:

  Whether to patch the signal only if it does not already
  exist. If not provided, the Datastar client side will default to false, which
  will cause the data to be patchd into the signals."
  common/only-if-missing)

;; Script opts
(def auto-remove
  "[[execute-script!]] option, boolean:

  Whether to remove the script after execution, if not
  provided the Datastar client side will default to true."
  common/auto-remove)

(def attributes
  "[[execute-script!]] option, map:

  A map of attributes to add to the script element."
  common/attributes)


;; -----------------------------------------------------------------------------
;; Data-star base api
;; -----------------------------------------------------------------------------
(def pm-outer
  "patch mode: replaces the outer HTML of the existing element."
  consts/element-patch-mode-outer)

(def pm-inner
  "patch mode: replaces the inner HTML of the existing element."
  consts/element-patch-mode-inner)

(def pm-remove
  "patch mode: remove the existing element from the dom."
  consts/element-patch-mode-remove)

(def pm-prepend
  "patch mode: prepends the element to the existing element."
  consts/element-patch-mode-prepend)

(def pm-append
  "patch mode: appends the element to the existing element."
  consts/element-patch-mode-append)

(def pm-before
  "patch mode: inserts the element before the existing element."
  consts/element-patch-mode-before)

(def pm-after
  "patch mode: inserts the element after the existing element."
  consts/element-patch-mode-after)

(def pm-replace
  "patch mode: Do not morph, simply replace the whole element and reset any
  related state."
  consts/element-patch-mode-replace)


(defn patch-elements!
  "Send HTML elements to the browser to be patchd into the DOM.

  Args:
  - `sse-gen`: the sse generator to send from
  - `elements`: A string of HTML elements.
  - `opts`: An options map

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[selector]]
  - [[patch-mode]]
  - [[use-view-transition]]

  Return value:
  - `false` if the connection is closed
  - `true` otherwise
  "
  ([sse-gen elements]
   (patch-elements! sse-gen elements {}))
  ([sse-gen elements opts]
   (elements/patch-elements! sse-gen elements opts)))


(defn patch-elements-seq!
  "Same as [[patch-elements!]] except that it takes a seq of elements."
  ([sse-gen elements]
   (patch-elements-seq! sse-gen elements {}))
  ([sse-gen elements opts]
   (elements/patch-elements-seq! sse-gen elements opts)))



(defn remove-element!
  "Remove element(s) from the dom. It is a convenience function using
  [[patch-elements!]] with the [[patch-mode]] options set to [[pm-remove]]
  and a [[selector]] set to `selector`.

  Args:
  - `sse-gen`: the sse generator to send from
  - `selector`: string, CSS selector that represents the elements to be
    removed from the DOM.
  - `opts`: options map

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[use-view-transition]]

  Return value:
  - `false` if the connection is closed
  - `true` otherwise
  "
  ([sse-gen selector]
   (remove-element! sse-gen selector {}))
  ([sse-gen selector opts]
   (elements/remove-element! sse-gen selector opts)))


(defn patch-signals!
  "
  Send signals to the browser using
  [RFC 7386 JSON Merge Patch](https://datatracker.ietf.org/doc/html/rfc7386)
  semantics.

   Args:
   - `sse-gen`: the sse generator to send from
   - `signals-content`: a JavaScript object or JSON string that will be sent to
      the browser to update signals. The data must evaluate to a
      valid JavaScript Object. `null` values for keys in this JSON object mean
      that the signal at these keys are to be removed.
   - `opts`: An options map

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[only-if-missing]]

  Return value:
  - `false` if the connection is closed
  - `true` otherwise
  "
  ([sse-gen signals-content]
   (patch-signals! sse-gen signals-content {}))
  ([sse-gen signals-content opts]
   (signals/patch-signals! sse-gen signals-content opts)))


(defn get-signals
  "Returns the signals json string from a ring request map.

  The Datastar signals are read from:
  - the `:query-params` key of of the ring request map for HTTP get requests
  - the body of the request for other HTTP methods

  (Bring your own json parsing)"
  [ring-request]
  (signals/get-signals ring-request))


(defn execute-script!
  "
  Construct a HTML script tag using `script-text` as its content. Then sends it
  to the brower using [[patch-elements!]] with [[patch-mode]] set to
  [[pm-append]] and [[selector]] set to `\"body\"`.

  The default behavior is to auto remove the script after it has run.

   Args:
  - `sse-gen`: the sse generator to send from
  - `script-text`: string that represents the JavaScript to be executed
    by the browser.
  - `opts`: An options map

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[auto-remove]] defaults to true
  - [[attributes]]

  Return value:
  - `false` if the connection is closed
  - `true` otherwise
  "
  ([sse-gen script-text]
   (scripts/execute-script! sse-gen script-text {}))
  ([sse-gen script-text opts]
   (scripts/execute-script! sse-gen script-text opts)))


 
;; -----------------------------------------------------------------------------
;; SSE helpers
;; -----------------------------------------------------------------------------
(defn- sse
  ([method url]
   (str "@" method "('" url "')"))
  ([method url opts-string]
   (str "@" method "('" url "', " opts-string ")")))


(defn sse-get
  "Helper making a @get(...) action."
  ([url]
   (sse "get" url))
  ([url opts-string]
   (sse "get" url opts-string)))


(defn sse-post
  "Helper making a @post(...) action."
  ([url]
   (sse "post" url))
  ([url opts-string]
   (sse "post" url opts-string)))


(defn sse-put
  "Helper making a @put(...) action."
  ([url]
   (sse "put" url))
  ([url opts-string]
   (sse "put" url opts-string)))


(defn sse-patch
  "Helper making a @patch(...) action."
  ([url]
   (sse "patch" url))
  ([url opts-string]
   (sse "patch" url opts-string)))


(defn sse-delete
  "Helper making a @delete(...) action."
  ([url]
   (sse "delete" url))
  ([url opts-string]
   (sse "delete" url opts-string)))


(comment
  (sse-get "/a/b")
  := "@get('/a/b')"

  (sse-put "/a/b" "{includeLocal: true}")
  := "@put('/a/b', {includeLocal: true})")


;; -----------------------------------------------------------------------------
;; Scripts common
;; -----------------------------------------------------------------------------
(defn console-log!
  "Log msg in the browser console.

  Same behavior as [[execute-script!]].
  "
  ([sse-gen msg]
   (console-log! sse-gen msg {}))
  ([sse-gen msg opts]
   (execute-script! sse-gen (str "console.log(\"" msg "\")") opts)))


(defn console-error!
  "Log error msg in the browser console.

  Same behavior as [[execute-script!]].
  "
  ([sse-gen msg]
   (console-error! sse-gen msg {}))
  ([sse-gen msg opts]
   (execute-script! sse-gen (str "console.error(\"" msg "\")") opts)))


(defn redirect!
  "Redirect a page using a script.

  Same behavior as [[execute-script!]].
  "
  ([sse-gen url]
   (redirect! sse-gen url {}))
  ([sse-gen url opts]
   (execute-script! sse-gen
                    (str "setTimeout(() => window.location.href =\"" url "\")")
                    opts)))


;; -----------------------------------------------------------------------------
;; Misc
;; -----------------------------------------------------------------------------
(defn datastar-request?
  "Test for the presence of the datastar header in a ring request."
  [request]
  (= "true" (get-in request [:headers "datastar-request"])))

