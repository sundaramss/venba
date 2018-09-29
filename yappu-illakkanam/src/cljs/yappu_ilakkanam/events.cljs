(ns yappu-ilakkanam.events
  (:require
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.db :as db]
   [yappu-ilakkanam.common :as common]
   [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::set-active-panel
 (fn-traced [db [_ active-panel]]
   (assoc db :active-panel active-panel)))

(re-frame/reg-event-db
 ::set-selected-opt
 (fn-traced [db [_ id value]]
   (let [idx (common/getOptIdx db id)
         search (:s db)]
     (assoc db :s (if (= value "0")
                      (reduce conj (subvec search 0 idx) (subvec search (inc idx)))
                      (if (nil? idx)
                        (conj search {:id id :opt value})
                        (update-in search [idx :opt] (fn [_] value))))))))
