(ns yappu-ilakkanam.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::active-page
 (fn [db _]
   (:active-page db)))

(re-frame/reg-sub
 ::get-page-type
 (fn [db _]
   (get-in db [:active-page :page] :pa)))

(re-frame/reg-sub
 ::pagination
 (fn [db _]
   (:pagination db)))

(re-frame/reg-sub
 ::get-padallist
 (fn [db _]
   (:padallist db)))

(re-frame/reg-sub
 ::filters-ids
 (fn [db _]
   (map #([(:id %) (:opt %)]) (get-in db [:active-page :paths]))))

(re-frame/reg-sub
  ::get-config-list
 (fn [db [_ seer]]
   (get-in db [:config seer] :asaiSeer)))

(re-frame/reg-sub
  ::get-eruthi-seer
 (fn [db [_]]
   (get-in db [:config :eruthiSeer] 0)))

(re-frame/reg-sub
  ::is-eruthi-seer
 (fn [db [_]]
   (let [eruthiSeer (get-in db [:config :eruthiSeer] 0)
         selseerid (get-in db [:selected :selseerid])]
     (= eruthiSeer selseerid))))

(re-frame/reg-sub
  ::get-seer-selection
 (fn [db [_]]
   (get-in db [:selected :seerlist])))

(re-frame/reg-sub
  ::get-selected
 (fn [db [_]]
   (get-in db [:selected])))

(re-frame/reg-sub
  ::get-pagination
 (fn [db [_]]
   (get-in db [:pagination])))

(re-frame/reg-sub
  ::get-pageno
 (fn [db [_]]
   (get-in db [:active-page :id])))

(re-frame/reg-sub
  ::get-total-cols
 (fn [db [_]]
   (get-in db [:config :lineCols] 4)))
   ;4))

(defn- find-and-get [mapcoll key]
   (first (filter (fn [[k v]] (= (name k) key)) mapcoll)))

(re-frame/reg-sub
  ::get-adhikaram-for
 (fn [db [_ [pal iyal adhikaram]]]
   (if (nil? adhikaram) {}
    (let [palMap (get-in db [:config :pal])
          iyalMap (get-in db [:config :iyal])
          adhikaramMap (get-in db [:config :adhikaram])
          palval (find-and-get palMap pal)
          iyalval (find-and-get iyalMap iyal)
          adhikaramval (find-and-get adhikaramMap adhikaram)]
      {:palno (:a (second palval)) :iyalno (:a (second iyalval)) :adhikaramno (second adhikaramval)}))))
