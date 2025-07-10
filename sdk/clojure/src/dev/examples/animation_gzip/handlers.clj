(ns examples.animation-gzip.handlers
  (:require
    [examples.animation-gzip.rendering :as rendering]
    [examples.animation-gzip.state :as state]
    [examples.utils :as u]
    [ring.util.response :as ruresp]
    [starfederation.datastar.clojure.adapter.common :as ac]))


(defn home-handler
  ([_]
   (ruresp/response (rendering/page @state/!state)))
  ([req respond _raise]
   (respond
     (home-handler req))))


(defn ->updates-handler
  [->sse-response & {:as opts}]
  (fn updates-handler
   ([req]
    (->sse-response req
      (merge opts
        {ac/on-open
         (fn [sse]
           (state/add-conn! sse))
         ac/on-close
         (fn on-close
           ([sse]
            (state/remove-conn! sse))
           ([sse _status]
            (on-close sse)))})))
   ([req respond _raise]
    (respond
      (updates-handler req)))))


(def id-regex #"[^-]*-(\d*)-(\d*)")


(defn recover-coords [req]
  (when-let [[_ x y] (-> req
                       :path-params
                       :id
                       (->> (re-find id-regex)))]
    {:x (Integer/parseInt x)
     :y (Integer/parseInt y)}))


(defn ping-handler
  ([req]
   (when-let [coords (recover-coords req)]
     (println "-- ping " coords)
     (state/add-ping! coords))
   {:status 204})
  ([req respond _raise]
   (respond (ping-handler req))))


(defn random-pings-handler
  ([_req]
   (println "-- add pixels")
   (state/add-random-pings!)
   {:status 204})
  ([req respond _raise]
   (respond
     (random-pings-handler req))))


(defn reset-handler
  ([_req]
   (println "-- reseting state")
   (state/reset-state!)
   {:status 204})
  ([req respond _raise]
   (respond (reset-handler req))))

(defn step-handler
  ([_req]
   (println "-- Step 1")
   (state/step-state!)
   {:status 204})
  ([req respond _raise]
   (respond
     (step-handler req))))



(defn play-handler
  ([_req]
   (println "-- play animation")
   (state/start-animating!)
   {:status 204})
  ([req respond _raise]
   (respond (play-handler req))))


(defn pause-handler
  ([_req]
   (println "-- pause animation")
   (state/stop-animating!)
   {:status 204})
  ([req respond _raise]
   (respond (pause-handler req))))

(defn resize-handler
  ([req]
   (let [{x "rows" y "columns"} (u/get-signals req)]
     (println "-- resize" x y)
     (state/resize! x y)
     {:status 204}))
  ([req respond _raise]
   (respond
     (resize-handler req))))


(defn refresh-handler
  ([_req]
   {:status 200
    :headers {"Content-Type" "text/html"}
    :body (rendering/render-content @state/!state)})
  ([req respond _raise]
   (respond (refresh-handler req))))


