(ns yappu-ilakkanam.subs
  (:require
   [yappu-ilakkanam.common :as common]
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::active-panel
 (fn [db _]
   (:active-panel db)))

(re-frame/reg-sub
 ::load-path
 (fn [db _]
   (:load-path db)))

(re-frame/reg-sub
 ::selected-opt
 (fn [db [_ id]]
   (common/getOpt db id)))

(re-frame/reg-sub
 ::get-kural
 (fn [db [_]]
   {
     :ve "1"
     :ave "1"
     :pal "அறத்துப்பால்"
     :iyal "பாயிரம்"
     :adhikaram "கடவுள் வாழ்த்து"
     :sp [[ [ "அக" "ர"]  [ "முத" "ல"]  ["எழுத்" "தெல்" "லாம்"] ["ஆ" "தி"]]
          [ ["பக" "வன்"] [ "முதற்" "றே"]  [ "உல" "கு"]]]
     :ap [[[ "NI" "NE" ]["NI" "NE"]["NI" "NE" "NE" ] [ "NE" "NE" ]] [[ "NI" "NE" ] [ "NI" "NE" ] [ "NI" "NE"]]]}))

(re-frame/reg-sub
 ::get-selected-opts
 (fn [db [_]]
   (let [opts (:s db)]
    (map (fn [{opt :opt id :id}] {:id id :val (map keyword (re-seq #".{1,2}" (or opt "")))}) opts))))
