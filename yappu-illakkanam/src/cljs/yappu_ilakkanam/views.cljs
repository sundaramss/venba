(ns yappu-ilakkanam.views
  (:require
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.subs :as subs]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.appcss :refer [H1 H-MENUS H-M-ROW H-MENU H-M-LINK
                                   CONTENT FILTER-CONTAINER K-F-TITLE K-F-WRAPPER K-F-DROPDOWN K-F-SELECT]]))

(def seerkal {1 [:NE] 2 [:NI] 3 [:NE :NE] 4 [:NI :NE]
              5 [:NE :NE] 6 [:NI :NE] 7 [:NI :NI] 8 [:NE :NI]
              9  [:NE :NE :NE] 10 [:NE :NE :NI] 11 [:NI :NE :NE] 12 [:NI :NE :NI]
              13 [:NI :NI :NE] 14 [:NI :NI :NI] 15 [:NE :NI :NE] 16 [:NE :NI :NI]
              17 [:NE :NE :NE :NE] 18 [:NE :NE :NE :NI] 19 [:NE :NE :NI :NE]  20 [:NE :NE :NI :NI]
              21 [:NI :NE :NE :NE] 22 [:NI :NE :NE :NI] 23 [:NI :NE :NI :NE]  24 [:NI :NE :NI :NI]
              25 [:NE :NI :NE :NE] 26 [:NE :NI :NE :NI] 27 [:NE :NI :NI :NE]  28 [:NE :NI :NI :NI]
              29 [:NI :NI :NE :NE] 30 [:NI :NI :NE :NI] 31 [:NI :NI :NI :NE]  32 [:NI :NI :NI :NI]})

(defn asai-seer [asaikal]
   (let [asaiJoin #(apply str (map name %))
         mudhal (condp contains? (asaiJoin (take 2 asaikal))
                 #{"NENE"} "தேமா"
                 #{"NINE"} "புளிமா"
                 #{"NINI"} "கருவிள"
                 #{"NENI"} "கூவிள")
         mudivuAsai  (last asaikal)
         mudivu (condp = (count asaikal)
                 1 ""
                 2 :>> (fn [_] (when (= :NI mudivuAsai) "ம்"))
                 3 :>> (fn [_] (condp = mudivuAsai
                                     :NE "ங்காய்"
                                     :NI "ங்கனி"))
                 4 :>> (fn [_] (condp contains? (asaiJoin (take-last 2 asaikal))
                                 #{"NENE"} "ந்தண்பூ"
                                 #{"NINE"} "நறும்பூ"
                                 #{"NINI"} "நறுநிழல்"
                                 #{"NENI"} "ந்தண்ணிழல்")))]
       (str mudhal mudivu)))

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
                        {:value x}
                        (asai-seer s)]))
                (map #(seerkal %) opts))]
  (fn []
   (let [field-value @(re-frame/subscribe [::subs/selected-opt id])]  ;(re-re-frame/subscribe [::subs/selected-opt id])]
     (into [:select
            {:class (K-F-SELECT)
             :value (or field-value 0)
             :on-change #(re-frame/dispatch [::events/set-selected-opt id (-> % .-target .-value)])}
            [:option "சீர்"]] options)))))

(defn kural-content []
  [:div {:class (CONTENT)}
    [:div {:class (FILTER-CONTAINER)}
     [:div {:class (K-F-WRAPPER)}
       [:h2 {:class (K-F-TITLE)} "தேடு"]
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
