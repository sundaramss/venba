(ns yappu-ilakkanam.views
  (:require
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.subs :as subs]
   [yappu-ilakkanam.appcss :refer [H1 LOGO H-MENUS H-M-ROW H-MENU H-M-LINK]]))

(defn header-link [title]
 (H-MENU (H-M-LINK title)))

(defn header-logo []
 [:div [:div (H1 {:color :dark} "YAPPU")]
    (H-MENUS
     (H-M-ROW
      (header-link "Home")
      (header-link "About")
      (header-link "Contact Us")))])
;; home

(defn home-panel []
  (header-logo))



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
