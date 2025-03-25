(ns examples.animation-gzip.brotli
  (:require
    [starfederation.datastar.clojure.adapter.common :as ac])
  (:import
    com.aayushatharva.brotli4j.Brotli4jLoader
    [com.aayushatharva.brotli4j.encoder  Encoder$Parameters
      Encoder$Mode BrotliOutputStream]
    [java.io OutputStream]))

;; This code is adapted from https://github.com/andersmurphy/hyperlith/blob/master/src/hyperlith/impl/brotli.clj
;; Thank you Anders!

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defonce ensure-br
  (Brotli4jLoader/ensureAvailability))


(defn encoder-params [{:keys [quality window-size]}]
  (let [encoder-params  (Encoder$Parameters/new)]
    (.setMode encoder-params Encoder$Mode/TEXT)
    (when window-size
      (.setWindow encoder-params window-size))
    (when quality
      (.setQuality encoder-params quality))
    encoder-params))


(defn ->brotli-os
  ([^OutputStream out-stream & {:as opts}]
   (BrotliOutputStream/new out-stream (encoder-params opts))))

(def brotli-content-encoding "br")

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def brotli-profile
  {ac/wrap-output-stream (fn [^OutputStream os]
                           (-> os ->brotli-os ac/->os-writer))
   ac/write! (ac/->write-with-temp-buffer!)
   ac/content-encoding brotli-content-encoding})


#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def brotli-buffered-writer-profile
  {ac/wrap-output-stream (fn [^OutputStream os]
                           (-> os ->brotli-os ac/->os-writer ac/->buffered-writer))
   ac/write! ac/write-to-buffered-writer!
   ac/content-encoding brotli-content-encoding})

