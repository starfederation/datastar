(ns starfederation.datastar.clojure.brotli
  "Tools to work with Brotli.

  The main api is
  - [[compress]]
  - [[decompress]]
  - [[->brotli-profile]]
  - [[->brotli-buffered-writer-profile]]
  "
  (:require
    [clojure.math :as m]
    [starfederation.datastar.clojure.adapter.common :as ac])
  (:import
    com.aayushatharva.brotli4j.Brotli4jLoader
    [com.aayushatharva.brotli4j.encoder
     Encoder
     Encoder$Parameters
     Encoder$Mode
     BrotliOutputStream]
    [com.aayushatharva.brotli4j.decoder Decoder]
    [java.io OutputStream]))


;; Code taken from https://github.com/andersmurphy/hyperlith
;; Thanks Anders!
;; -----------------------------------------------------------------------------
;; Setup & helpers
;; -----------------------------------------------------------------------------
(defonce ensure-br
  (Brotli4jLoader/ensureAvailability))


(defn window-size->kb [window-size]
  (/ (- (m/pow 2 window-size) 16) 1000))


(defn encoder-params
  "Options used when creating a brotli encoder.

  Arg keys:
  - `:quality`: Brotli quality defaults to 5
  - `:window-size`: Brotli window size defaults to 24
  "
  [{:keys [quality window-size]}]
  (doto (Encoder$Parameters/new)
    (.setMode Encoder$Mode/TEXT)
    ;; LZ77 window size (0, 10-24) (default: 24)
    ;; window size is (pow(2, NUM) - 16)
    (.setWindow (or window-size 24))
    (.setQuality (or quality 5))))


;; -----------------------------------------------------------------------------
;; 1 shot compression
;; -----------------------------------------------------------------------------
(defn compress
  "
  Compress `data` (either a byte array or a string) using Brotli.

  Opts keys from [[encoder-params]]:
  - `:quality`: Brotli quality
  - `:window-size`: Brotli window size
  "

  [data & {:as opts}]
  (-> (if (string? data)
        (String/.getBytes data)
        ^byte/1 data)
      (Encoder/compress (encoder-params opts))))


(defn decompress
  "Decompress Brotli compressed data, returns a string."
  [data]
  (let [decompressed (Decoder/decompress data)]
    (String/new (.getDecompressedData decompressed))))


(comment
  (decompress (compress "hello")))


;; -----------------------------------------------------------------------------
;; Write profiles
;; -----------------------------------------------------------------------------
(defn ->brotli-os
  "Wrap `out-stream` with Brotli compression.

  Opts from [[encoder-params]]:
  - `:quality`: Brotli quality
  - `:window-size`: Brotli window size
  "
  [^OutputStream out-stream & {:as opts}]
  (BrotliOutputStream/new out-stream (encoder-params opts)))


(def brotli-content-encoding "br")


(defn ->brotli-profile
  "Make a write profile using Brotli compression and a temporary buffer
  strategy.

  Opts from [[encoder-params]]:
  - `:quality`: Brotli quality
  - `:window-size`: Brotli window size
  "
  [& {:as opts}]
  {ac/wrap-output-stream
   (fn [^OutputStream os]
     (-> os
         (->brotli-os opts)
         ac/->os-writer))
   ac/write! (ac/->write-with-temp-buffer!)
   ac/content-encoding brotli-content-encoding})


(defn ->brotli-buffered-writer-profile
  "Make a write profile using Brotli compression and a permanent buffer
  strategy.

  Opts from [[encoder-params]]:
  - `:quality`: Brotli quality
  - `:window-size`: Brotli window size
  "
  [& {:as opts}]
  {ac/wrap-output-stream
   (fn [^OutputStream os]
     (-> os
         (->brotli-os opts)
         ac/->os-writer
         ac/->buffered-writer))
   ac/write! ac/write-to-buffered-writer!
   ac/content-encoding brotli-content-encoding})


