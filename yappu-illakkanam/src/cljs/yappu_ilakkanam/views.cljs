(ns yappu-ilakkanam.views
  (:require
   [re-frame.core :as re-frame]
   [reagent.core :as reagent]
   [clojure.string :as cstr]
   [yappu-ilakkanam.common :as common]
   [yappu-ilakkanam.subs :as subs]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.routes :refer [set-hash!]]))

(defn padal-menu []
  (fn []
   [:nav.navbar.is-info
    [:div.navbar-start
     [:div.navbar-item.has-dropdown.is-hoverable.is-info
      [:a.navbar-link "இ"]
      [:div.navbar-dropdown
       [:a.navbar-item "திருக்குறள்"]]]]]))
(defn header []
 (let [burgerClicked (reagent/atom false)
       burgerHandler (fn [e] (reset! burgerClicked (not @burgerClicked)))]
  (fn []
   [:header.columns.has-background-info.is-vcentered.is-mobile
    [:div.column.is-one-quarter-desktop.is-two-quarters-tablet.is-three-quarters-mobile
     [:span.is-size-3 [:strong.has-text-white "யாப்பு"]]
     [:span.has-text-white "- திருக்குறள்"]]
    [:div.column.is-three-quarters.is-hidden-touch
     [padal-menu]]
    [:div.column.is-hidden-desktop
     [:a.navbar-burger.burger
      {:on-click burgerHandler,
       :role "button"}
      [:span {:aria-hidden "true"}]
      [:span {:aria-hidden "true"}]
      [:span {:aria-hidden "true"}]]]])))

(defn new-filter-option [idx eruthiSeer]
 (let [ opts (if (= idx eruthiSeer) [1 2 3 4] [5 6 7 8 9 10 11 12])
       seerfn (if (= idx eruthiSeer) common/etru-seer common/asai-seer)
       xf  (comp (map common/seerkal)
                 (map (fn [s] [s (apply str (map name s))]))
                 (map (fn [[k v]] [:option {:key v :value v} (seerfn k)])))]
  (transduce xf conj opts)))

(defn new-fitler [eruthiSeer]
 (let [options7 (new-filter-option eruthiSeer eruthiSeer)
       options (new-filter-option 1 eruthiSeer)
       defaultValue7 (get-in options7 [0 1 :value])
       defaultValue (get-in options [0 1 :value])
       totalSeer eruthiSeer
       selectedSeerIdx  (reagent/atom "1")
       selectedSeer  (reagent/atom defaultValue)]
  (fn []
    [:filter.is-vcentered.columns.is-gapless
     [:div.column.is-1]
     [:div.column.is-two-fifths-tablet.is-half-mobile
      [:p.is-hidden-tablet.is-hidden_mobile.bd-notification.is-size-4.has-text-weight-semibold  "சல்லடை"]
      [:div.columns.is-mobile.is-variable.is-1-tablet.is-1-mobile.is-vcentered
       [:div.column]
       [:div.column.is-hidden-400px
         [:span.is-size-4.has-text-weight-semibold "சல்லடை"]]
       [:div.column
         [:div.field.has-addons
           [:div.control.has-icons-left
              [:div.select.is-rounded.is-size-7-mobile
               [:select {:on-change #(reset! selectedSeerIdx (.. % -target -value))}
                [:option {:selected true :value 1} "சீர் - 1"]
                (for [x (range 2 (+ totalSeer 1))]
                   ^{:key x}[:option {:value x} (str "சீர் - " x)])]
               [:span.icon.is-small.is-left [:i.fa.fa-list-ol]]]]
           [:div.control.has-icons-left
             [:div.select.is-rounded.is-size-7-mobile
              [(fn [id]
                (let [is7Idx (= id (str eruthiSeer))]
                 (reset! selectedSeer (if is7Idx defaultValue7 defaultValue))
                 (into [:select {:on-change #(reset! selectedSeer (.. % -target -value))}]
                  (if is7Idx options7 options)))) @selectedSeerIdx]
              [:span.icon.is-small.is-left [:i.fa.fa-list]]]]]]
       [:div.column
         [:a.button {:on-click #(re-frame/dispatch [::events/set-selected-opt (js/parseInt @selectedSeerIdx) @selectedSeer])}
          [:span.icon.is-small>i.fa.fa-search]]]]]])))

(defn filter-tag [tag eruthiSeer]
 (let [{id :id v :val} tag
       is7Idx (= id eruthiSeer)
       seerfn (if is7Idx common/etru-seer common/asai-seer)
       value  (seerfn v)]
  [:div.control
   [:div.tags.has-addons
    [:span.tag.is-dark id]
    [:span.tag.is-warning.has-text-weight-semibold value]
    [:a.tag.is-delete.has-background-link.has-text-white
      {:on-click #(re-frame/dispatch [::events/set-selected-opt id "0"])}]]]))

(defn filter-tags [eruthiSeer]
 (let [opts @(re-frame/subscribe [::subs/get-selected-opts])]
  [:div.columns.is-gapless
   [:div.column.is-1]
   [:div.column.is-four-fifths
    [:div.card
       [:header.card-header.has-background-info
        [:p.card-header-title.has-text-white "சிறப்பு"]
        [:a.card-header-icon.has-text-white
         {:href "#"}
         (if (not-empty opts) [:span.icon.is-info [:i.fa.fa-trash]])]]
       [:div.card-content
        [:div.content
          (if (empty? opts) "எதையும் தேர்வு செய்யவில்லை"
           (into [:div.field.is-grouped.is-grouped-multiline] (map #(filter-tag % eruthiSeer) opts)))]]]]
   [:div.column.is-1]]))


(defn get-pagination [end current]
 (cond
  (or (< end current) (< current 1)) []
  (<= (dec end) 4) (vec (range 1 (inc end)))
  (>= 3 current) [1 2 3 4 nil end]
  (< (- end 3) current) (into [1 nil] (range (- end 3) (inc end)))
  :else [1 nil (dec current) current (inc current) nil end]))

(defn adhikaram-href [padal value]
 (if (nil? value) "" (str "#/" padal "/" value)))

(defn search-href [padal path value]
 (if (nil? path) "" (str "#/" padal "/s/" path "?p=" value)))

(defn new-pagination [padal]
 (let [{pagination :pagination
        searchPage :searchPage
        searchPath :searchPath} @(re-frame/subscribe [::subs/get-pagination])
        {current :current endPage :total-pages} pagination
        pagelist (get-pagination endPage current)
        linkFn (if (nil? searchPage) (partial adhikaram-href padal) (partial search-href padal searchPath))]
  [:div.columns.is-marginless.is-paddingless.is-gapless
   [:div.column.is-1]
   [:div.column.is-half
    [:nav.pagination
     [:ul.pagination-list
      (map #(cond
             (nil? %)[:li [:span.pagination-ellipsis ".."]]
             :else [:li {:key %} [:a.pagination-link
                                  {:class (when (= % current) "is-current")
                                   :href (linkFn %)} %]])
           pagelist)]]]]))
(defn kural-path-view [{pal :pal iyal :iyal adhikaram :adhikaram} id]
 (when-let [pal pal]
  (let [path [pal iyal adhikaram]]
   [:nav.breadcrumb.is-small-mobile.mobile-font-size
     [:ul
      (map (fn [i] ^{:key i}[:li [:a {:href "#"} i]]) path)
      [:li.is-active [:span {:style {:padding-left "0.5rem"}} id]]]])))

(defn oru-sol-1 [idx sol mudivuSol?]
  (let [[sp ap] sol
        seerfn (if mudivuSol? common/etru-seer common/asai-seer)]
   [:div.has-background-white-bis {:key idx} [:span.has-text-weight-bold.has-text-black (cstr/join "/" sp)]
    [:br]
    [:span.is-italic (cstr/join "/" (map #((keyword %) common/asai) ap))]
    [:br]
    [:span.has-span.has-text-grey-dark.has-text-weight-bold (seerfn (map #(keyword %) ap))]]))

(defn oru-adi-1 [idx, adi mudivuAdi? solEnneekai]
 (let [mudivuSol (when mudivuAdi? (dec (count adi)))
       newadi (common/padding adi solEnneekai [[]])]
    (map-indexed #(oru-sol-1 (str idx %1) %2 (= mudivuSol %1)) newadi)))

(defn one-padal-1 [{padal :padal idx :id}]
 (let [sp (:sp padal)
       ap (:ap padal)
       etruSeerIrrukku true
       adikalEnneekai (count sp)
       solEnneekai (apply max (map count sp))
       mudivuAdiIdx (dec adikalEnneekai)
       variakal (for [x (range adikalEnneekai)] (map vector (get sp x) (get ap x)))]
  (when (not-empty sp)
   [:article.message.is-info {:key  idx}
     [:div.columns.is-mobile.is-gapless.is-marginless
      [:div.column.is-1.is-hidden-400px]
      [:div.column.is-four-fifths-tablet.is-half-desktop
       [:div.message-header   [kural-path-view padal idx]]]
      [:div.column.is-1.is-hidden-400px]]
    [:div.message-body.is-paddingless.zero-border-width
     [:div.columns.is-mobile.is-gapless
      [:div.column.is-1.is-hidden-400px]
      [:div.column.is-four-fifths-tablet.is-half-desktop.has-background-grey-lighter
       [:div.grid
        (map-indexed #(oru-adi-1 (str idx %1) %2 (= mudivuAdiIdx %1) solEnneekai) variakal)]]
      [:div.column.is-1.is-hidden-400px]]]])))


(defn kural-list-1 []
 (let [klist @(re-frame/subscribe [::subs/get-kural])]
  [:div.section.is-paddingless
   (map #(one-padal-1 %) klist)]))

;; home
(defn home-panel []
 (let [{eruthiSeer :eruthiSeer padal :padal} @(re-frame/subscribe [::subs/get-init])
       np [new-pagination padal]]
  [:div.section.is-paddingless
    [header]
    [new-fitler eruthiSeer]
    [filter-tags eruthiSeer]
    (into [] np)
    [kural-list-1]
    (into [] np)]))


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
