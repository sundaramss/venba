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
   (:fetchdata db)))

(re-frame/reg-sub
 ::get-selected-opts
 (fn [db [_]]
   (let [opts (:s db)]
    (map (fn [{opt :opt id :id}] {:id id :val (map keyword (re-seq #".{1,2}" (or opt "")))}) opts))))

(defn- build-path [db]
  (if (nil? (:s db))
      db
      (assoc db :searchPath (reduce (fn [r seer](str r "/" (:id seer) "/" (:opt seer))) "" (:s db)))))

(re-frame/reg-sub
 ::get-pagination
 (fn [db [_]]
   (->  db
      (build-path)
      (select-keys  [:pagination :searchPage :searchPath]))))
