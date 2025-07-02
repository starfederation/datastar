(ns starfederation.datastar.clojure.brotli-test
  (:require
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.test :as at]
    [starfederation.datastar.clojure.adapter.common-test :as ct]
    [starfederation.datastar.clojure.brotli :as brotli]
    [lazytest.core :as lt :refer [defdescribe describe specify expect]])
  (:import
    [java.io InputStream ByteArrayOutputStream
     ByteArrayInputStream InputStreamReader BufferedReader]
    [java.nio.charset StandardCharsets]))



(defn ->input-stream-reader [^InputStream is]
  (InputStreamReader. is StandardCharsets/UTF_8))


(defn ->ba [v]
  (cond
    (bytes? v)
    v

    (instance? ByteArrayOutputStream v)
    (.toByteArray ^ByteArrayOutputStream v)))


(defn read-bytes [ba opts]
  (if (:brotli opts)
    (brotli/decompress ba)
    (-> ba
        ->ba
        (ByteArrayInputStream.)
        (->input-stream-reader)
        (BufferedReader.)
        (slurp))))

(defdescribe reading-bytes
  (specify "We can do str -> bytes -> str"
    (let [original (str (d*/patch-elements! (at/->sse-gen) "msg"))]
      (expect
        (= original
           (-> original
             (.getBytes)
             (read-bytes {})))))))

;; -----------------------------------------------------------------------------
;; Tests
;; -----------------------------------------------------------------------------
(defn simple-round-trip [write-profile]
  (let [!res (atom nil)
        machinery (ct/->machinery write-profile)
        baos (ct/get-baos machinery)]
    (with-open [_baos baos
                writer (ct/get-writer machinery)]
      (ct/append-then-flush writer "some text"))
    (reset! !res (-> baos .toByteArray (read-bytes write-profile)))
    (expect (= @!res "some text"))))



(defdescribe brotli
  (describe "Writing of text with compression"
    (specify "We can do a simple round trip"
      (simple-round-trip (assoc (brotli/->brotli-profile)
                                :brotli true)))

    (specify "We can compress several messages"
      (let [machinery (ct/->machinery (brotli/->brotli-profile))
            baos (ct/get-baos machinery)
            !res (atom [])]
          (with-open [writer (ct/get-writer machinery)]
            (ct/append-then-flush writer "some text")
            (ct/append-then-flush writer "some other text"))
          (reset! !res (-> baos .toByteArray (read-bytes {:brotli true})))
          (expect (= @!res "some textsome other text"))))))



(comment
  (require '[lazytest.repl :as ltr])
  (ltr/run-test-var #'reading-bytes)
  (ltr/run-test-var #'brotli))




