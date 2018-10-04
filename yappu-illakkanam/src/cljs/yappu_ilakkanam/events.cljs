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
 ::search-kural
 (fn-traced [db [_ path]]
  (let [spath (re-seq #"((\d+)/([^/]+))" path)
        search (reduce
                (fn [r x] (let [[p q] (subvec x 2)] (conj r {:id (js/parseInt p) :opt q})))
                [] spath)]
   (if (empty? search)
     (assoc db :load-path "kural/1")
     (-> db
      (assoc :s search)
      (dissoc :kural-adhikaram)
      (assoc :active-panel :kural-panel))))))

(re-frame/reg-event-db
 ::load-kural
 (fn-traced [db [_ aidx]]
  (let [idx (js/parseInt (or (not-empty (clojure.string/join (re-seq #"\d+" aidx))) "1"))]
   (-> db
    (assoc :s [])
    (assoc :kural-adhikaram idx)
    (assoc :active-panel :kural-panel)))))

(re-frame/reg-event-db
 ::set-selected-opt
 (fn-traced [db [_ id value]]
   (let [idx (common/getOptIdx db id)
         search (:s db)
         nsearch (if (= value "0")
                     (reduce conj (subvec search 0 idx) (subvec search (inc idx)))
                     (if (nil? idx)
                        (conj search {:id id :opt value})
                        (update-in search [idx :opt] (fn [_] value))))
         path (reduce (fn [r seer](str r "/" (:id seer) "/" (:opt seer))) "" nsearch)]
      (if (empty? path) (assoc db :load-path "/kural/1") (assoc db :load-path (str "/kural/s" path))))))
