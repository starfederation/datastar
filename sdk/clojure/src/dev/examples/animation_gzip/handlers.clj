(ns examples.animation-gzip.handlers
  (:require
    [examples.animation-gzip.rendering :as rendering]
    [examples.animation-gzip.state :as state]
    [ring.util.response :as ruresp]
    [starfederation.datastar.clojure.api :as d*]
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


(defn ->ping-handler [->sse-response]
  (fn ping-handler
    ([req]
     (->sse-response req
       {:status 204
        ac/on-open
        (fn [sse]
          (d*/with-open-sse sse
            (when-let [coords (recover-coords req)]
              (state/add-ping! coords))))}))
    ([req respond _raise]
     (respond
       (ping-handler req)))))


(defn ->random-pings-handler [->sse-response]
  (fn random-pings-handler
    ([req]
     (->sse-response req
       {:status 204
        ac/on-open
        (fn [sse]
          (d*/with-open-sse sse
            (state/add-random-pings!)))}))
    ([req respond _raise]
     (respond
       (random-pings-handler req)))))


(defn ->reset-handler [->sse-response]
  (fn reset-handler
    ([req]
     (->sse-response req
       {:status 204
        ac/on-open
        (fn [sse]
          (d*/with-open-sse sse
            (state/reset-state!)))}))
    ([req respond _raise]
     (respond
       (reset-handler req)))))


(defn ->play-handler [->sse-response]
  (fn play-handler
    ([req]
     (->sse-response req
       {:status 204
        ac/on-open
        (fn [sse]
          (d*/with-open-sse sse
            (state/start-animating!)))}))
    ([req respond _raise]
     (respond
       (play-handler req)))))


(defn ->pause-handler [->sse-response]
  (fn pause-handler
    ([req]
     (->sse-response req
       {:status 204
        ac/on-open
        (fn [sse]
          (d*/with-open-sse sse
            (state/stop-animating!)))}))
    ([req respond _raise]
     (respond
       (pause-handler req)))))


(defn ->refresh-handler [->sse-response & {:as opts}]
  (fn refresh-handler
    ([req]
     (->sse-response req
       (merge opts
         {ac/on-open
          (fn [sse]
            (d*/with-open-sse sse
              (d*/merge-fragment! sse
                (rendering/render-content @state/!state))))})))
    ([req respond _raise]
     (respond
       (refresh-handler req)))))



