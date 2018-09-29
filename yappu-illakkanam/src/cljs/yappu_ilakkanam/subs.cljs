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
 ::selected-opt
 (fn [db [_ id]]
   (common/getOpt db id)))

(re-frame/reg-sub
 ::get-selected-opts
 (fn [db [_]]
   (let [opts (:s db)]
    (map (fn [{opt :opt}] (map keyword (re-seq #".{1,2}" (or opt "")))) opts))))
