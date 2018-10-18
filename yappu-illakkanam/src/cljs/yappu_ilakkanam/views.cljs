(ns yappu-ilakkanam.views
  (:require
   [re-frame.core :as re-frame]
   [clojure.string :as cstr]
   [yappu-ilakkanam.common :as common]
   [yappu-ilakkanam.subs :as subs]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.routes :refer [set-hash!]]
   [yappu-ilakkanam.appcss :refer [H1 H-MENUS H-M-ROW H-MENU H-M-LINK
                                   CONTENT FILTER-CONTAINER K-F-TITLE K-F-WRAPPER K-F-DROPDOWN K-F-SELECT
                                   CHIP C-CHIP P-C-CHIP CROSS EN
                                   KURALS KURAL-LIST KURAL KURAL-PATH KURAL-PATH-C K-P-ITEM K-ADI K-ADI-1 K-ADI-2 K-A-S-1 K-A-SEER
                                   UL-P UL-P-TOP UL-P-BOTTOM UL-LI-P UL-LI-A-P]]))

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
 (let [seerfn (if (= id 7) common/etru-seer common/asai-seer)
       options (map (fn [s]
                     (let [x (apply str (map name s))]
                      [:option
                        {:key x :value x}
                        (seerfn s)]))
                (map #(common/seerkal %) opts))
       dropdown (into [:select
                       {:class (K-F-SELECT)
                        :on-change #(re-frame/dispatch [::events/set-selected-opt id (-> % .-target .-value)])}
                       [:option {:value 0} (str "சீர்-" id )]] options)]
  (fn []
   (let [field-value @(re-frame/subscribe [::subs/selected-opt id])]  ;(re-re-frame/subscribe [::subs/selected-opt id])]
      (update dropdown 1 #(assoc % :value (or field-value 0)))))))

(defn show-selected-opt []
  (let [opts @(re-frame/subscribe [::subs/get-selected-opts])]
   (reduce #(conj %1 [:li {:class (CHIP)}
                      [:span {:class (EN)} (%2 :id)]
                      [:span (common/asai-seer (%2 :val))]
                      [:button {:class (CROSS)
                                :on-click (fn [e]
                                           (re-frame/dispatch [::events/set-selected-opt (%2 :id) "0"]))}
                             "x"]])
          [:ul {:class (C-CHIP)}] opts)))
   ;(map #([:span (apply str %)]) opts)))

(defn- drop-option [idx]
 [:div {:class (K-F-DROPDOWN) :key idx}
   [filter-option idx (if (= idx 7) [1 2 3 4] [5 6 7 8 9 10 11 12])]])

(defn kural-filter []
  [:div {:class (CONTENT)}
    [:div {:class (FILTER-CONTAINER)}
     [:div {:class (K-F-WRAPPER)}
       [:div {:class (P-C-CHIP)}
        [:h2 {:class (K-F-TITLE)} "தேடு"]
        [show-selected-opt]]
       (map #(drop-option %) (range 1 8))]]])

(defn kural-path-view [{pal :pal iyal :iyal adhikaram :adhikaram} id]
  [:div {:class (KURAL-PATH)}
    [:span {:class (K-P-ITEM)} pal]
    [:span {:class (K-P-ITEM)} (str " > " iyal)]
    [:span {:class (K-P-ITEM)} (str " > " adhikaram)]
    [:span {:class (K-P-ITEM)} (str " > " id)]])

(defn adi-view [n idx v]
   [:span {:class (K-A-SEER (= idx 0)) :key (str "ad" idx n)} (cstr/join "/" v)])

(defn asai-view [n idx v]
   [:span {:class (K-A-S-1 (= idx 0)) :key (str "as" idx n)} (cstr/join "/" (map #((keyword %) common/asai) v))])

(defn seer-view [n idx v]
 (let [sf (if (and (= n 1) (= idx 2)) common/etru-seer common/asai-seer)]
   [:span {:class (K-A-S-1 (= idx 0)) :key (str "as" idx n)} (sf (map #(keyword %) v))]))

(defn kural-adi-view [n sp]
 (let [adi (get sp n)]
  [[:div {:class (K-ADI) :key n}]
   (concat []
     (map-indexed #(adi-view n %1 %2) adi))]))

(defn kural-asai-view [n ap]
 (let [adi (get ap n)]
   [[:div {:class (K-ADI-1) :key (str "as" n)}
     (concat []
       (map-indexed #(asai-view n %1 %2) adi))]]))

(defn kural-seer-view [n ap]
 (let [adi (get ap n)]
   [[:div {:class (K-ADI-2) :key (str "ase" n)}
     (concat []
       (map-indexed #(seer-view n %1 %2) adi))]]))

(defn k-v [x k]
 (concat []
      (kural-adi-view x (:sp k))
      (kural-asai-view x (:ap k))
      (kural-seer-view x (:ap k))))

(defn kural-one [{k :padal id :id}]
  [:div {:class (KURAL) :key id}
    [:div {:class (KURAL-PATH-C)}
          (kural-path-view k id)
          (for [x (range (count (:sp k)))]
              (k-v x k))]])

(defn kural-adhikaram-list []
  [:div {:class (KURALS)}
    [:div {:class (KURAL-LIST)}
      (let [klist @(re-frame/subscribe [::subs/get-kural])]
         (map #(kural-one %1) klist))]])

(defn adhikaram-href [value]
 (if (nil? value) "" (str "#/kural/" value)))

(defn kural-pagination []
  [:ul {:class (UL-P)}
   (let [pagination @(re-frame/subscribe [::subs/get-pagination])
         ul-li-p (UL-LI-P)
         activeF (UL-LI-A-P false)
         activeT (UL-LI-A-P true)
         {current :current previous :previous
           next :next pages :pages} pagination
         pval (when previous (dec current))
         nval (when next (inc current))
         litem (fn [value href & [label]]
                 (if (nil? value) ""
                  [:li {:class ul-li-p :key (or value label)}
                   [:a {:class (if (= current value) activeT activeF) :href href} (or label value)]]))]
        (concat [] [(litem pval (adhikaram-href (or pval "#")) "«")
                    (map #(litem % (adhikaram-href %)) pages)
                    (litem nval (adhikaram-href (or nval "#")) "»")]))])


(defn home-panel []
 (let [pagination (kural-pagination)]
  [:div
    (header)
    [:div {:style {:position "relative"}}
     (kural-filter)
     [:div {:class (UL-P-TOP)} pagination]]
    [:div {:style {:position "relative"}}
     (kural-adhikaram-list)
     [:div {:class (UL-P-BOTTOM)} pagination]]]))


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
    :kural-panel [home-panel]
    :about-panel [about-panel]
    [:div]))

(defn- init-re-load []
  (let [path @(re-frame/subscribe [::subs/load-path])]
   (when (not-empty path) (set-hash! path))))

(defn show-panel [panel-name]
  [panels panel-name])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    (init-re-load)
    [show-panel @active-panel]))
