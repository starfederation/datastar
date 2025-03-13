(ns test.examples.counter
  (:require
    [test.examples.common :as common]
    [dev.onionpancakes.chassis.core :as h]
    [dev.onionpancakes.chassis.compiler :as hc]
    [ring.util.response :as rur]
    [starfederation.datastar.clojure.adapter.common :as ac]
    [starfederation.datastar.clojure.api :as d*]
    [test.utils :as u]))

;; -----------------------------------------------------------------------------
;; Views
;; -----------------------------------------------------------------------------
(defn ->->id [prefix]
 (fn [s]
   (str prefix s)))

(def ->inc-id (->->id "increment-"))
(def ->dec-id (->->id "decrement-"))

(defn inc-url [id]
  (str "/counters/increment/" id))

(defn dec-url [id]
  (str "/counters/decrement/" id))


(defn sse-inc [counter-id]
  (d*/sse-get (inc-url  counter-id)))

(defn sse-inc-post [counter-id]
  (d*/sse-post (inc-url  counter-id)))

(defn signal-inc [counter-id]
  (format "$%s += 1" counter-id))

(defn sse-dec [counter-id]
  (d*/sse-get (dec-url counter-id)))

(defn sse-dec-post [counter-id]
  (d*/sse-post (dec-url  counter-id)))


(defn signal-dec [counter-id]
  (format "$%s -= 1" counter-id))


(defn counter-button [id text action]
  (hc/compile
    [:button
     {:id id
      :data-on-click action}
     text]))


(defn counter [id & {:keys [inc dec]
                     :or {inc sse-inc
                          dec sse-dec}}]
  (let [counter-id (str "counter" id)]
    (hc/compile
      [:div {(keyword (str "data-signals-" counter-id)) "0"}
       (counter-button (->inc-id id) "inc" (inc counter-id))
       (counter-button (->dec-id id) "dec" (dec counter-id))
       [:span {:id counter-id
               :data-text (str "$"counter-id)}]])))


(defn counter-page []
  (common/scaffold
    (hc/compile
      [:div
       [:h2 "Counter page"]
       [:div
        [:h3 "Server side with get"]
        (counter "1")]
       [:div
        [:h3 "Server side with post"]
        (counter "2" :inc sse-inc-post :dec sse-dec-post)]
       [:div
        [:h3 "Client side"]
        (counter "3" :inc signal-inc :dec signal-dec)]])))


(def page (h/html (counter-page)))


;; -----------------------------------------------------------------------------
;; Handlers common logic
;; -----------------------------------------------------------------------------
(defn counters
  ([_]
   (rur/response page))
  ([_ respond _]
   (respond (rur/response page))))


(defn update-signal* [req f & args]
  (let [signals (->  req d*/get-signals u/read-json)
        id (-> req :path-params :id)
        val (get signals id)]
    (format "{'%s':%s}" id (apply f val args))))


(defn ->update-signal [->sse-response]
  (fn update-signal [req f & args]
    (->sse-response req
      {ac/on-open (fn [sse-gen]
                    (d*/merge-signals! sse-gen (apply update-signal* req f args))
                    (d*/close-sse! sse-gen))})))


