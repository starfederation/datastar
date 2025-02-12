(ns
  ^{:doc "Public api for the Datastar SDK.

    The api consists of 5 main functions that operate on SSE generators, see:
    - [[merge-fragment!]]
    - [[remove-fragment!]]
    - [[merge-signals!]]
    - [[remove-signals!]]
    - [[execute-script!]]

    These function take options map whose keys are:
    - [[id]]
    - [[retry-duration]]
    - [[selector]]
    - [[merge-mode]]
    - [[settle-duration]]
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
    - [[redirect!]]
    "}
  starfederation.datastar.clojure.api
  (:require
    [starfederation.datastar.clojure.api.common :as common]
    [starfederation.datastar.clojure.api.fragments :as fragments]
    [starfederation.datastar.clojure.api.signals :as signals]
    [starfederation.datastar.clojure.api.scripts :as scripts]
    [starfederation.datastar.clojure.consts :as consts]
    [starfederation.datastar.clojure.protocols :as p]))


;; -----------------------------------------------------------------------------
;; SSE generator management
;; -----------------------------------------------------------------------------
(defn close-sse!
  "Close the connection of a sse generator."
  [sse-gen]
  (p/close-sse! sse-gen))


(defmacro with-open-sse
  "Macro functioning similarly to [[clojure.core/with-open]]. It evalutes the
  `body` inside a try expression and closes the `sse-gen` at the end using
  [[close-see!]] in a finally clause.

  Ex:
  ```
  (with-open-sse sse-gen
    (d*/merge-fragment! sse-gen frag1)
    (d*/merge-signals!  sse-gen signals))
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

;; Merge fragment opts
(def selector
  "[[merge-fragment!]] option, string:

  The CSS selector to use to insert the fragments. If not
  provided or empty, Datastar will default to using the id attribute of the
  fragment."
  common/selector)

(def merge-mode
  "[[merge-fragment!]] option, string:

  The mode to use when merging fragments into the DOM.
  If not provided the Datastar client side will default to morph.

  The set of valid values is:
  - [[mm-morph]]
  - [[mm-inner]]
  - [[mm-outer]]
  - [[mm-prepend]]
  - [[mm-append]]
  - [[mm-before]]
  - [[mm-after]]
  - [[mm-upsert-attributes]]
  "
  common/merge-mode)

(def settle-duration
  "[[merge-fragment!]] / [[remove-fragment!]] option, number:

  Used to control the amount of time that a fragment
  should take before removing any CSS related to settling. It is used to allow
  for animations in the browser via the Datastar client. If provided the value must
  be a positive integer of the number of milliseconds to allow for settling. If none
  is provided, the default value of 300 milliseconds will be used."
  common/settle-duration)

(def use-view-transition
  "[[merge-fragment!]] / [[remove-fragment!]  option, boolean:

  Whether to use view transitions, if not provided the
  Datastar client side will default to false."
  common/use-view-transition)

;;Signals opts
(def only-if-missing
  "[[merge-signals!]] option, boolean:

  Whether to merge the signal only if it does not already
  exist. If not provided, the Datastar client side will default to false, which
  will cause the data to be merged into the signals."
  common/only-if-missing)

;; Script opts
(def auto-remove
  "[[execute-script!]] option, boolean:

  Whether to remove the script after execution, if not
  provided the Datastar client side will default to true."
  common/auto-remove)

(def attributes
  "[[execute-script!]] option, map:

  A map of attributes to add to the script element,
  if not provided the Datastar client side will default to
  `{:type \"module\"}`."
  common/attributes)


;; -----------------------------------------------------------------------------
;; Data-star base api
;; -----------------------------------------------------------------------------
(def mm-morph
  "Merge mode: morphs the fragment into the existing element using idiomorph."
  consts/fragment-merge-mode-morph)

(def mm-inner
  "Merge mode: replaces the inner HTML of the existing element."
  consts/fragment-merge-mode-inner)

(def mm-outer
  "Merge mode: replaces the outer HTML of the existing element."
  consts/fragment-merge-mode-outer)

(def mm-prepend
  "Merge mode: prepends the fragment to the existing element."
  consts/fragment-merge-mode-prepend)

(def mm-append
  "Merge mode: appends the fragment to the existing element."
  consts/fragment-merge-mode-append)

(def mm-before
  "Merge mode: inserts the fragment before the existing element."
  consts/fragment-merge-mode-before)

(def mm-after
  "Merge mode: inserts the fragment after the existing element."
  consts/fragment-merge-mode-after)

(def mm-upsert-attributes
  "Merge mode: upserts the attributes of the existing element."
  consts/fragment-merge-mode-upsert-attributes)


(defn merge-fragment!
  "Send HTML fragments to the browser to be merged into the DOM.

  Args:
  - `sse-gen`: the sse generator to send from
  - `fragments`: A string of HTML fragments.
  - `opts`: An options map

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[selector]]
  - [[merge-mode]]
  - [[settle-duration]]
  - [[use-view-transition]]
  "
  ([sse-gen fragments]
   (merge-fragment! sse-gen fragments {}))
  ([sse-gen fragments opts]
   (fragments/merge-fragment! sse-gen fragments opts)))


(defn merge-fragments!
  "Same as [[merge-fragment!]] except that it takes a seq of fragments.
  "
  ([sse-gen fragments]
   (merge-fragments! sse-gen fragments {}))
  ([sse-gen fragments opts]
   (fragments/merge-fragments! sse-gen fragments opts)))



(defn remove-fragment!
  "Send a selector to the browser to remove HTML fragments from the DOM.

  Args:
  - `sse-gen`: the sse generator to send from
  - `selector`: string, CSS selector that represents the fragments to be
    removed from the DOM.
  - `opts`: options map

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[settle-duration]]
  - [[use-view-transition]]
  "
  ([sse-gen selector]
   (remove-fragment! sse-gen selector {}))
  ([sse-gen selector opts]
   (fragments/remove-fragment! sse-gen selector opts)))


(defn merge-signals!
  "Send one or more signals to the browser to be merged into the signals.

   Args:
   - `sse-gen`: the sse generator to send from
   - `signals-content`: a JavaScript object or JSON string that will be sent to
      the browser to update signals in the signals. The data must evaluate to a
      valid JavaScript. It will be converted to signals by the Datastar client
      side.
  - `opts`: An options map

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[only-if-missing]]
  "
  ([sse-gen signals-content]
   (merge-signals! sse-gen signals-content {}))
  ([sse-gen signals-content opts]
   (signals/merge-signals! sse-gen signals-content opts)))


(defn remove-signals!
  "Send signals to the browser to be removed from the signals.

  Args:
  - `sse-gen`: the sse generator to send from
  - `paths`: seq of strings that represent the signal paths to be removed from
    the signals. The paths must be valid `.` delimited paths to signals within the
    signals. The Datastar client side will use these paths to remove the data
    from the signals.

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[only-if-missing]]
  "
  ([sse-gen paths]
   (signals/remove-signals! sse-gen paths {}))
  ([sse-gen paths opts]
   (signals/remove-signals! sse-gen paths opts)))


(defn get-signals
  "Returns the signals json string from a ring request map.
  (Bring your own json parsing)"
  [ring-request]
  (signals/get-signals ring-request))


(defn execute-script!
  "
  Send an execute script event to the client.

   Args:
  - `sse-gen`: the sse generator to send from
  - `script-content`: string that represents the JavaScript to be executed
    by the browser.
  - `opts`: An options map

  Options keys:
  - [[id]]
  - [[retry-duration]]
  - [[auto-remove]]
  - [[attributes]]
  "
  ([sse-gen script-content]
   (scripts/execute-script! sse-gen script-content {}))
  ([sse-gen script-content opts]
   (scripts/execute-script! sse-gen script-content opts)))


 
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
  "Log msg in the browser console."
  ([sse-gen msg]
   (console-log! sse-gen msg {}))
  ([sse-gen msg opts]
   (execute-script! sse-gen (str "console.log(\"" msg "\")") opts)))


(defn console-error!
  "Log error msg in the browser console."
  ([sse-gen msg]
   (console-error! sse-gen msg {}))
  ([sse-gen msg opts]
   (execute-script! sse-gen (str "console.error(\"" msg "\")") opts)))


(defn redirect!
  "Redirect a page using a script."
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

