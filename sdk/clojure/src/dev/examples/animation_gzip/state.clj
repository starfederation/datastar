(ns examples.animation-gzip.state
  (:require
    [examples.animation-gzip.animation :as animation]))

;; -----------------------------------------------------------------------------
;; Animation state
;; -----------------------------------------------------------------------------
(defonce !state (atom animation/starting-state))


(defn reset-state! []
  (reset! !state animation/starting-state))


(defn add-ping!
  ([pos]
   (swap! !state animation/add-ping pos))
  ([pos duration speed]
   (swap! !state animation/add-ping pos duration speed)))


(defn add-random-pings! []
  (swap! !state animation/add-random-pings 10))


(defn step-state! []
  (swap! !state animation/step-state))


(defn- try-claiming  [current-state id]
  (compare-and-set!
    !state
    current-state
    (animation/start-animating current-state id)))


(defn- claim-animator-job! [id]
  (let [current-state @!state]
    (if (:animator current-state)
      :already-claimed
      (if (try-claiming current-state id)
        :claimed
        (recur id)))))


(defn start-animating! []
  (let [id (random-uuid)
        res (claim-animator-job! id)]
    (when (= :claimed res)
      (future
        (loop []
          (let [state @!state]
            (when (:animator state)
              (step-state!)
              (Thread/sleep (long (:animation-tick state)))
              (recur))))))))


(defn stop-animating! []
  (swap! !state animation/stop-animating))


;; -----------------------------------------------------------------------------
;; SSE connections state
;; -----------------------------------------------------------------------------
(defonce !conns (atom #{}))


(defn add-conn! [sse]
  (swap! !conns conj sse))


(defn remove-conn! [sse]
  (swap! !conns disj sse))


