(ns examples.redirect
 (:require
    [examples.common :as c]
    [examples.utils :as u]
    [dev.onionpancakes.chassis.core :refer [html]]
    [reitit.ring :as rr]
    [ring.util.response :as ruresp]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]))

;; Redirection example

(def home-page
  (html
    (c/page-scaffold
     [[:h1 "Test page"]
      [:div.#indicator
       [:button {:data-on-click (d*/sse-get "/redirect-me")}
        "Start redirect"]]])))


(defn home [_]
  (ruresp/response home-page))


(def guide-page
  (html
    (c/page-scaffold
     [[:h1 "You have been redirected"]
      [:a {:href "/" } "Home"]])))


(defn guide [_]
  (ruresp/response guide-page))


(defn redirect-handler [ring-request]
  (->sse-response ring-request
    {on-open
      (fn [sse]
        (d*/patch-elements! sse
          (html [:div#indicator "Redirecting in 3 seconds..."]))
        (Thread/sleep 3000)
        (d*/redirect! sse "/guide")
        (d*/close-sse! sse))}))




(def router (rr/router
              [["/" {:handler home}]
               ["/guide" {:handler guide}]
               ["/redirect-me" {:handler redirect-handler}]
               c/datastar-route]))


(def default-handler (rr/create-default-handler))


(def handler
  (rr/ring-handler router default-handler))



(comment
  (u/reboot-hk-server! #'handler))


