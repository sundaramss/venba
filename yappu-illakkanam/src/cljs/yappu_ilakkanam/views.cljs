(ns yappu-ilakkanam.views
  (:require
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.subs :as subs]
   [yappu-ilakkanam.appcss :refer [h1]]))



;; home

(defn home-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [h1 {:size "32px" :color :dark :padding-v "8px" :padding-h "4px" :margin "8px 16px"} (str "Hello from " @name ". This is the Home Page.")]
     [:div
      [:a {:href "#/about"}
       "go to About Page"]]]))



;; about

(defn about-panel []
  [:div
   [:h1 "This is the About Page."]

   [:div
    [:a {:href "#/"}
     "go to Home Page"]]])


;; main

(defn- panels [panel-name]
  (case panel-name
    :home-panel [home-panel]
    :about-panel [about-panel]
    [:div]))

(defn show-panel [panel-name]
  [panels panel-name])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    [show-panel @active-panel]))
