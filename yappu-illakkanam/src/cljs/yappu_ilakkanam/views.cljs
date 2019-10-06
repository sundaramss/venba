(ns yappu-ilakkanam.views
  (:require
   [re-frame.core :as re-frame]
   [reagent.core :as reagent]
   [yappu-ilakkanam.routes :as routes]
   [yappu-ilakkanam.subs :as subs]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.helper :as helper]))

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

(defn new-fitler []
    [:div.is-vcentered.columns.is-gapless
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
               [:select {:defaultValue 1 :on-change #(re-frame/dispatch [::events/set-seer-id-selection (js/parseInt (.. % -target -value))])}
                [:option {:value 1} "சீர் - 1"]
                (for [x (range 2 (+ @(re-frame/subscribe [::subs/get-eruthi-seer]) 1))]
                   ^{:key x}[:option {:value x} (str "சீர் - " x)])]
               [:span.icon.is-small.is-left [:i.fa.fa-list-ol]]]]
           [:div.control.has-icons-left
             [:div.select.is-rounded.is-size-7-mobile
               [:select {:on-change #(re-frame/dispatch [::events/set-seer-selection (.. % -target -value)])}
                 (let [selseer @(re-frame/subscribe [::subs/get-seer-selection])]
                  (doall (for [[key value] selseer]
                          ^{:key key}[:option {:value value} (helper/display-value value)])))]
              [:span.icon.is-small.is-left [:i.fa.fa-list]]]]]]
       [:div.column
         [:a.button {:on-click #(helper/do-filter)}
          [:span.icon.is-small>i.fa.fa-search]]]]]])

(defn filter-tag [tag]
 (let [{id :id value :value} tag]
  [:div.control
   [:div.tags.has-addons
    [:span.tag.is-dark id]
    [:span.tag.is-warning.has-text-weight-semibold value]
    [:a.tag.is-delete.has-background-link.has-text-white
      {:on-click #(helper/get-remove-filter-url id)}]]]))

(defn filter-tags []
 (let [opts (helper/get-seleted-opts)]
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
           (into [:div.field.is-grouped.is-grouped-multiline] (map #(filter-tag %) opts)))]]]]
   [:div.column.is-1]]))


(defn new-pagination []
 (let [pagelist (helper/get-pagination-list)]
  [:div.columns.is-marginless.is-paddingless.is-gapless
   [:div.column.is-1]
   [:div.column.is-half
    [:nav.pagination
     (into
      [:ul.pagination-list]
      (map (let [n (volatile! 1)]
             (fn [{is-current? :is-current? href :href pageno :pageno}]
               (cond
                (nil? pageno)[:li {:key (str "pg" (vswap! n inc)) :id @n} [:span.pagination-ellipsis ".."]]
                :else [:li {:key pageno} [:a.pagination-link
                                           {:class (when is-current? "is-current")
                                            :on-click #(when (not is-current?) (routes/set-token! href))
                                            :href (if is-current? "javascript:void(0)"  href)} pageno]])))
          pagelist))]]]))

(defn show-breadcrumbs [id breadcrumbs]
   [:nav.breadcrumb.is-small-mobile.mobile-font-size
     [:ul
       (map (fn [{idx :id name :name url :url}]
               ^{:key (str name "-" idx)}
               [:li [:a {:href url :on-click #(routes/set-token! url)} name]]) breadcrumbs)
      [:li.is-active [:span {:style {:padding-left "0.5rem"}} id]]]])

(defn sols [idx lineno line]
 (let [colsLen @(re-frame/subscribe [::subs/get-total-cols])
       solsLen (count line)
       y   (map-indexed
              (fn [colid [sp ap sol]]
               [:div.has-background-white-bis {:key (str idx "-" lineno "-" colid)}
                [:span.has-text-weight-bold.has-text-black (clojure.string/join "/" sp)]
                [:br]
                [:span.is-italic (clojure.string/join "/" ap)]
                [:br]
                [:span.has-span.has-text-grey-dark.has-text-weight-bold sol]])
            line)]
   (if (= colsLen solsLen) y (concat y [[:div.has-background-white-bis {:key "9999"}]]))))

(defn one-padal [padal]
 (let [{id :id
        lines :lines
        breadcrumbs :breadcrumbs} padal]
  (when (not-empty lines)
   [:article.message.is-info {:key  id}
     [:div.columns.is-mobile.is-gapless.is-marginless
      [:div.column.is-1.is-hidden-400px]
      [:div.column.is-four-fifths-tablet.is-half-desktop
       [:div.message-header   [show-breadcrumbs id breadcrumbs]]]
      [:div.column.is-1.is-hidden-400px]]
    [:div.message-body.is-paddingless.zero-border-width
     [:div.columns.is-mobile.is-gapless
      [:div.column.is-1.is-hidden-400px]
      [:div.column.is-four-fifths-tablet.is-half-desktop.has-background-grey-lighter
       (into [:div.grid]
         (map-indexed (partial sols id) lines))]
      [:div.column.is-1.is-hidden-400px]]]])))

(defn show-padals []
  (let [padallist (helper/get-padals)]
   (into
    [:div.section.is-paddingless]
    (map #(one-padal %) padallist))))

(defn main-panel
  []
  (let [page (re-frame/subscribe [::subs/get-page-type])]
    [:div.section.is-paddingless
      [header]
      [new-fitler]
      [filter-tags]
      [new-pagination]
      [show-padals]
      [new-pagination]]))
