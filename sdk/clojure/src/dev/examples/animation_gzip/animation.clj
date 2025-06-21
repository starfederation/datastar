(ns examples.animation-gzip.animation
  (:require
    [clojure.math :as math]))


;; -----------------------------------------------------------------------------
;; Basic math
;; -----------------------------------------------------------------------------
(defn point [x y]
  {:x x :y y})

(defn distance [p1 p2]
  (let [x1 (:x p1)
        y1 (:y p1)
        x2 (:x p2)
        y2 (:y p2)]
    (math/sqrt (+
                (math/pow (- x2 x1) 2)
                (math/pow (- y2 y1) 2)))))


(defn clamp [low n high]
  (min high (max low n)))

(defn clamp-color [v]
  (clamp 0 v 255))

(comment
  (clamp-color -1)
  (clamp-color 100)
  (clamp-color 300))

;; -----------------------------------------------------------------------------
;; State management
;; -----------------------------------------------------------------------------
(defn next-color [current]
  (case current
    :r :g
    :g :b
    :b :r))


(def starting-state
  {:animator nil
   :animation-tick 100
   :clock 0
   :size {:x 10 :y 10}
   :color :r
   :pings []})



(def default-ping-duration 20)
(def default-ping-speed 0.5)


(defn resize [state x y]
  (assoc state :size {:x x :y y}))


(defn ->ping [state pos duration speed]
  {:clock (:clock state)
   :color (:color state)
   :duration duration
   :speed speed
   :traveled 0
   :pos pos})


(defn add-ping
  ([state pos]
   (add-ping state pos default-ping-duration default-ping-speed))
  ([state pos duration speed]
   (-> state
       (update :color next-color)
       (update :pings conj (->ping state pos duration speed)))))


(defn add-random-pings [state n]
  (let [size (:size state)
        x (:x size)
        y (:y size)]
    (reduce
      (fn [acc _]
        (add-ping acc (point (inc (rand-int x))
                             (inc (rand-int y)))))
      state
      (range n))))


(defn keep-ping? [general-clock ping]
  (-> (:clock ping)
      (+ (:duration ping))
      (- general-clock)
      pos?))


(defn traveled-distance [general-clock ping-clock speed]
  (let [elapsed-time (- general-clock ping-clock)]
    (int (math/floor (* speed elapsed-time)))))


(defn update-ping [general-clock ping]
  (let [c (:clock ping)
        s (:speed ping)]
    (assoc ping
      :traveled (traveled-distance general-clock c s))))

(defn ->x-update-pings [general-clock]
  (comp
    (filter #(keep-ping? general-clock %))
    (map #(update-ping general-clock %))))


(defn start-animating [state id]
  (assoc state :animator id))


(defn stop-animating [state]
  (dissoc state :animator))


(defn step-state [state]
  (let [new-clock (-> state :clock inc)
        pings (:pings state)
        x-update-pings (->x-update-pings new-clock)
        new-pings (into [] x-update-pings pings)]
    (-> state
        transient
        (assoc! :clock new-clock
                :color (next-color (:color state))
                :pings new-pings)
        persistent!
        (cond->
          (empty? new-pings) stop-animating))))


