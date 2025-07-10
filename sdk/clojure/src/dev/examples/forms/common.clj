(ns examples.forms.common
  (:require
    [dev.onionpancakes.chassis.compiler :as hc]))


(defn result-area [res-from-signalsl res-from-form]
  (hc/compile
    [:div {:id "form-result"}
     [:span "From signals: " [:span {:data-text "$input1"}]]
     [:br]
     [:span "from backend signals: " [:span  res-from-signalsl]]
     [:br]
     [:span "from backend form: " [:span  res-from-form]]]))

