(ns yappu-ilakkanam.views
  (:require
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.common :as common]
   [yappu-ilakkanam.subs :as subs]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.appcss :refer [H1 H-MENUS H-M-ROW H-MENU H-M-LINK
                                   CONTENT FILTER-CONTAINER K-F-TITLE K-F-WRAPPER K-F-DROPDOWN K-F-SELECT]]))

(defn header-link [title]
 [:li {:class (H-MENU)}
      [:a {:class (H-M-LINK)} title]])

(defn header []
 [:div
    [H1 {:color :dark} "யாப்பு"]
    [:div {:class (H-MENUS)}
     [:div {:class (H-M-ROW)}
       [header-link "Home"]
       [header-link "About"]
       [header-link "Contact Us"]]]])
;; home
(defn filter-option [id opts]
 (let [options (map (fn [s]
                     (let [x (apply str (map name s))]
                      [:option
                        {:key x :value x}
                        (common/asai-seer s)]))
                (map #(common/seerkal %) opts))
       dropdown (into [:select
                       {:class (K-F-SELECT)
                        :on-change #(re-frame/dispatch [::events/set-selected-opt id (-> % .-target .-value)])}
                       [:option {:value 0} "சீர்"]] options)]
  (fn []
   (let [field-value @(re-frame/subscribe [::subs/selected-opt id])]  ;(re-re-frame/subscribe [::subs/selected-opt id])]
      (update dropdown 1 #(assoc % :value (or field-value 0)))))))

(defn show-selected-opt []
  (let [opts @(re-frame/subscribe [::subs/get-selected-opts])]
   (reduce #(conj %1 [:span (common/asai-seer %2)]) [:p] opts)))

   ;(map #([:span (apply str %)]) opts)))

(defn kural-content []
  [:div {:class (CONTENT)}
    [:div {:class (FILTER-CONTAINER)}
     [:div {:class (K-F-WRAPPER)}
       [:h2 {:class (K-F-TITLE)} "தேடு"]
       [show-selected-opt]
       [:div {:class (K-F-DROPDOWN)}
           [filter-option 1 [5 7 8]]]
       [:div {:class (K-F-DROPDOWN)}
           [filter-option 2 [5 9 10 16 30]]]
       [:div {:class (K-F-DROPDOWN)}
           [filter-option 3 [5 7 8]]]
       [:div {:class (K-F-DROPDOWN)}
           [filter-option 4 [5 9 10 16 30]]]
       [:div {:class (K-F-DROPDOWN)}
           [filter-option 5 [5 7 8]]]
       [:div {:class (K-F-DROPDOWN)}
           [filter-option 6 [5 9 10 16 30]]]
       [:div {:class (K-F-DROPDOWN)}
           [filter-option 7 [5 9 10 16 30]]]]]])


(defn home-panel []
  [:div
    (header)
    (kural-content)])


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
