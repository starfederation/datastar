(ns starfederation.datastar.clojure.api.scripts
  (:require
    [starfederation.datastar.clojure.api.common   :as common]
    [starfederation.datastar.clojure.api.elements :as elements]
    [starfederation.datastar.clojure.consts       :as consts]))


(defn ->script-tag [script opts]
  (let [auto-remove (common/auto-remove opts)
        attrs       (common/attributes opts)
        script-tag-builder (StringBuilder.)]

    ;; Opening
    (.append script-tag-builder "<script")

    ;; Adding script tag attrs
    (when attrs
      (doseq [[k v] attrs]
        (.append script-tag-builder " ")
        (.append script-tag-builder (name k))
        (.append script-tag-builder "=\"")
        (.append script-tag-builder (str v))
        (.append script-tag-builder "\"")))

    ;; Adding auto-remove logic
    (when (or (nil? auto-remove) auto-remove)
      (.append script-tag-builder " data-effect=\"el.remove()\""))

    ;; Opening done
    (.append script-tag-builder ">")

    ;; Content of the script
    (.append script-tag-builder script)

    ;; Closing
    (.append script-tag-builder "</script>")

    ;; Returning the built tag
    (str script-tag-builder)))


(def patch-opts
  {common/selector "body"
   common/patch-mode consts/element-patch-mode-append})


(defn execute-script! [sse-gen script-text opts]
  (elements/patch-elements! sse-gen
                            (->script-tag script-text opts)
                            (merge opts patch-opts)))

(comment
  (= (->script-tag "console.log('hello')" {})
     "<script data-on-load=\"el.remove()\">console.log('hello')</script>")

  (= (->script-tag "console.log('hello')"
                    {common/auto-remove false})
     "<script>console.log('hello')</script>")


  (= (->script-tag "console.log('hello')"
                  {common/auto-remove false
                   common/attributes {:type "module"}})
     "<script type=\"module\">console.log('hello')</script>")


  (= (->script-tag "console.log('hello');\nconsole.log('world!!!')"
                  {common/auto-remove :true
                   common/attributes {:type "module" :data-something 1}})
     "<script type=\"module\" data-something=\"1\" data-on-load=\"el.remove()\">console.log('hello');\nconsole.log('world!!!')</script>"))


