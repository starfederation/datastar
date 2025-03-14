(ns starfederation.datastar.clojure.adapter.common-test
  (:require
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.adapter.test :as at]
    [lazytest.core :as lt :refer [defdescribe describe specify expect]])
  (:import
    [java.io
     Writer InputStream ByteArrayOutputStream
     ByteArrayInputStream InputStreamReader BufferedReader]
    [java.nio.charset StandardCharsets]
    [java.util.zip GZIPInputStream]))


;; -----------------------------------------------------------------------------
;; Reading helpers
;; -----------------------------------------------------------------------------
(defn ->input-stream-reader [^InputStream is]
  (InputStreamReader. is StandardCharsets/UTF_8))


(defn ->ba [v]
  (cond
    (bytes? v)
    v

    (instance? ByteArrayOutputStream v)
    (.toByteArray ^ByteArrayOutputStream v)))


(defn read-bytes [ba opts]
  (-> ba
      ->ba
      (ByteArrayInputStream.)
      (cond-> (:gzip? opts) (GZIPInputStream.))
      (->input-stream-reader)
      (BufferedReader.)
      (slurp)))


(defdescribe reading-bytes
  (specify "We can do str -> bytes -> str"
    (let [original (str (d*/merge-fragment! (at/->sse-gen) "msg"))]
      (expect
        (= original
           (-> original
             (.getBytes)
             (read-bytes {})))))))

;; -----------------------------------------------------------------------------
;; Test helpers
;; -----------------------------------------------------------------------------
(defn ->machinery [write-profile]
  (let [^ByteArrayOutputStream baos (ByteArrayOutputStream.)
        {write! ac/write-to-buffered-writer!
         wrap   ac/wrap-output-stream} write-profile
        writer (wrap baos)]
    {:write! write!
     :writer writer
     :baos baos}))

(defn get-baos ^ByteArrayOutputStream [machinery]
  (:baos machinery))


(defn get-writer ^Writer [machinery]
  (:writer machinery))


(defn append-then-flush [writer s]
  (doto ^Writer writer
    (.append (str s))
    (.flush)))


;; -----------------------------------------------------------------------------
;; Tests
;; -----------------------------------------------------------------------------
(defn simple-round-trip [write-profile]
  (let [!res (atom nil)
        machinery (->machinery write-profile)
        baos (get-baos machinery)]
    (with-open [_baos baos
                writer (get-writer machinery)]
      (append-then-flush writer "some text"))
    (reset! !res (-> baos .toByteArray (read-bytes write-profile)))
    (expect (= @!res "some text"))))


(defn resetless-writes [opts]
  (let [!res (atom [])
        machinery (->machinery opts)]
    (with-open [baos (get-baos machinery)
                writer (get-writer machinery)]

      (append-then-flush writer "some text")
      (swap! !res conj (-> baos .toByteArray (read-bytes opts)))

      (append-then-flush writer "some other text")
      (swap! !res conj (-> baos .toByteArray (read-bytes opts))))

    (expect (= @!res ["some text" "some textsome other text"]))))


(defn writes [opts]
  (let [!res (atom [])
        machinery (->machinery opts)]
    (with-open [baos (get-baos machinery)
                writer (get-writer machinery)]

      (append-then-flush writer "some text")
      (swap! !res conj (-> baos .toByteArray (read-bytes opts)))

      (.reset baos)

      (append-then-flush writer "some other text")
      (swap! !res conj (-> baos .toByteArray (read-bytes opts))))
    (expect (= @!res ["some text" "some other text"]))))


(defdescribe normal
  (describe "Writing of text without compression"
    (specify "We can do a simple round trip"
      (simple-round-trip ac/basic-profile))


    (describe "We need to be careful about reseting the ouput stream"
      (specify "Without reset"
        (resetless-writes ac/basic-profile))

      (specify "With reset"
        (writes ac/basic-profile)))))


(defdescribe gzip
  (describe "Writing of text with compression"
    (specify "We can do a simple round trip"
      (simple-round-trip (assoc ac/gzip-profile
                                :gzip? true)))

    (specify "We can compress several messages"
      (let [machinery (->machinery ac/gzip-profile)
            baos (get-baos machinery)
            !res (atom [])]
          (with-open [writer (get-writer machinery)]
            (append-then-flush writer "some text")
            (append-then-flush writer "some other text"))
          (reset! !res (-> baos .toByteArray (read-bytes {:gzip? true})))
          (expect (= @!res "some textsome other text"))))))


(comment
  :dbg
  :rec
  *e
  (require '[lazytest.repl :as ltr])
  (ltr/run-test-var #'reading-bytes)
  (ltr/run-test-var #'normal)
  (ltr/run-test-var #'gzip))

