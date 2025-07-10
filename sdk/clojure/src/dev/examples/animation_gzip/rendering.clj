(ns examples.animation-gzip.rendering
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [dom-top.core :as dt]
    [examples.common :as c]
    [examples.animation-gzip.animation :as animation]
    [starfederation.datastar.clojure.api :as d*]))

;; -----------------------------------------------------------------------------
;; Rendering util
;; -----------------------------------------------------------------------------
(defn color-level [distance]
  (case (int distance)
    0 100
    1 50
    2 20
    0))


(defn modify-color ^long [x intensity]
  (animation/clamp-color (- x intensity)))


(defn compute-intensity [ping pixel-position]
  (let [ping-traveled-distance (:traveled ping)
        cell-to-ping-distance (animation/distance pixel-position (:pos ping))
        delta (-> (- ping-traveled-distance cell-to-ping-distance)
                  abs math/floor int)]
    (color-level delta)))


#_ {:clj-kondo/ignore true}
(defn cell-color [state pos]
  (let [general-clock (:clock state)]
    (dt/loopr [r 255
               g 255
               b 255]
              [ping (:pings state)]
      (let [intensity (compute-intensity ping pos)]
        (if (pos? intensity)
          (case (:color ping)
            :r (recur r (modify-color g intensity) (modify-color b intensity))
            :g (recur (modify-color r intensity) g (modify-color b intensity))
            :b (recur (modify-color r intensity) (modify-color g intensity) b))
          (recur r g b)))
      [r g b])))



;; -----------------------------------------------------------------------------
;; Page generation
;; -----------------------------------------------------------------------------
(def css (slurp (io/resource "examples/animation_gzip/style.css")))

(defn rgb [r g b]
  (str "rgb(" r ", " g ", " b")"))


(defn cell-style [v]
  (str "background-color: "v";}"))


(def on-click
  "@get(`/ping/${event.srcElement.id}`)")


(defn pseudo-pixel [state x y]
  (let [c  (cell-color state (animation/point x y))
        id (str "px-" x "-" y)]
    (hc/compile
      [:div.pseudo-pixel {:style (cell-style (apply rgb c))
                          :id id}
       " "])))

(defn grid-style [state]
  (let [columns (-> state :size :y)]
    (str "grid-template-columns: repeat(" columns ", 1fr)")))


#_ {:clj-kondo/ignore true}
(defn pseudo-canvas [state]
  (let [size (:size state)
        rows (:x size)
        columns (:y size)]
    (dt/loopr
      [pc (transient [:div.pseudo-canvas {:style (grid-style state)
                                          :data-on-click on-click}])]
      [r (range 1 (inc rows))
       c (range 1 (inc columns))]
      (recur (conj! pc (pseudo-pixel state r c)))
      (persistent! pc))))


(defn left-pane [state]
  (hc/compile
    [:div#left-pane
     (pseudo-canvas state)]))



(defn controls [state]
  (hc/compile
    [:div
     [:h3 "Controls"]
     [:ul.h-list
      [:li [:button {:data-on-click (d*/sse-get "/refresh")} "refresh"]]
      [:li [:button {:data-on-click (d*/sse-get "/reset")} "reset"]]
      [:li [:button {:data-on-click (d*/sse-get "/random-10")} "add 10"]]
      [:li [:button {:data-on-click (d*/sse-get "/step1")} "step1"]]
      (if (:animator state)
        (hc/compile
          [:li [:button {:data-on-click (d*/sse-get "/pause")} "pause"]])
        (hc/compile
          [:li [:button {:data-on-click (d*/sse-get "/play")} "play"]]))]

     [:ul.h-list {:data-signals-rows    (-> state :size :x)
                  :data-signals-columns (-> state :size :y)}
      [:li "rows: "    [:input {:type "number"
                                :data-bind "rows"
                                :data-on-change (d*/sse-post "/resize")}]]
      [:li "columns: " [:input {:type "number"
                                :data-bind "columns"
                                :data-on-change (d*/sse-post "/resize")}]]]]))


(defn log-pane [state]
  (hc/compile
    [:div#log-pane.stack
     [:h3 "State"]
     [:div.stack
      [:h4 "General state"]
      [:pre (pr-str (:size state))]
      [:span "clock: " (:clock state)]
      [:span "animator:" (:animator state)]]

     (let [pings (:pings state)]
       (when (seq pings)
         [:div
          [:h4 "Pings"]
          [:table
           [:thead
            [:tr
             [:th "clock"] [:th "color"] [:th "duration"] [:th "speed"]
             [:th "traveled"] [:th "pos"]]]
           [:tbody
            (for [ping pings]
              (hc/compile
                [:tr
                 [:td (:clock ping)]
                 [:td (:color ping)]
                 [:td (:duration ping)]
                 [:td (:speed ping)]
                 [:td (:traveled ping)]
                 [:td [:pre (pr-str (:pos ping))]]]))]]]))]))



(defn right-pane [state]
  [:div#right-pane.stack.center
   (controls state)
   (log-pane state)])


(defn content [state]
  [:div#main-content.center
   (left-pane state)
   (right-pane state)])


(defn page [state]
  (h/html
    (c/page-scaffold
      [:div {:data-on-load (d*/sse-get "/updates")}
       [:style (h/raw css)]
       [:h2.center "lets get something fun going"]
       (content state)])))


(defn render-content [state]
  (h/html (content state)))
