(ns example.core
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [dev.onionpancakes.chassis.compiler :as hc]
    [dev.onionpancakes.chassis.core :as h]
    [example.utils :as u]
    [reitit.ring.middleware.parameters :as rmparams]
    [reitit.ring :as rr]
    [ring.util.response :as ruresp]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.ring-jetty :refer [->sse-response]]))


(def home-page
  (-> (io/resource "public/hello-world.html")
      slurp
      (string/split-lines)
      (->> (drop 3)
           (apply str))))


(defn home [_]
  (-> home-page
      (ruresp/response)
      (ruresp/content-type "text/html")))


(def message "Hello, world!")

(def msg-count  (count message))


(defn ->frag [i]
  (h/html
    (hc/compile
      [:div {:id "message"}
       (subs message 0 (inc i))])))



(defn hello-world [request]
  (let [d (-> request u/get-signals (get "delay") int)]
    (->sse-response request
      {:on-open
       (fn [sse]
         (d*/with-open-sse sse
           (dotimes [i msg-count]
             (d*/merge-fragment! sse (->frag i))
             (Thread/sleep d))))})))


(def routes
  [["/" {:handler home}]
   ["/hello-world" {:handler hello-world
                    :middleware [rmparams/parameters-middleware]}]])

(def router (rr/router routes))

(def handler (rr/ring-handler router))

