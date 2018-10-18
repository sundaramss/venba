(ns yappu-ilakkanam.events
  (:require
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.db :as db]
   [yappu-ilakkanam.common :as common]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
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
      (dissoc :kural-adhikaram :ar)
      (assoc :active-panel :kural-panel))))))

;(defn- kural-filter-effect [id path]
;  {:method :get
;   :uri  (str "/kural/" path "/" id ".json")
;   :response-format (ajax/json-response-format {:keywords? true})
;   :on-success [:kural-filter-received id]
;   :on-failure [:kural-filter-not-received id]})

(defn- kural-req-effect [idx {kid :id}]
   {:method :get
    :uri  (str "/kural/extra/" kid ".json")
    :response-format (ajax/json-response-format {:keywords? true})
    :on-success [:kural-received idx kid]
    :on-failure [:kural-not-received idx kid]})


(re-frame/reg-event-db
  :kural-received
  (fn [db [_ idx id res]]
    (update-in db [:fetchdata idx :padal] (fn [_] res))))


(re-frame/reg-event-db
  :kural-not-received
  (fn [db [_ id res]]
    (prn "Failure:" id res)
    db))

(def kural-fetch
   (re-frame/->interceptor
     :id :kural-fetch
     :after (fn [context]
              (let [fd (get-in context [:effects :db :fetchdata])
                    httpeffects (map-indexed kural-req-effect fd)]
               (if (not-empty httpeffects)
                   (assoc-in context [:effects :http-xhrio] httpeffects)
                   context)))))

(defn- prepare-fetchdata [sidx]
 (let [krange  (let [x (+ (* sidx 10) 1)] (range x (+ x 10)))]
  (reduce (fn [r x] (conj r (assoc {} :id x :padal nil))) [] krange)))

(re-frame/reg-event-db
 ::load-range-kural
 [kural-fetch]
 (fn-traced [db [_ aidx]]
  (let [ sidx (dec aidx)]
   (-> db
    (assoc :s [])
    (assoc :kural-adhikaram aidx)
    (assoc :fetchdata (prepare-fetchdata sidx))
    (assoc :pagination (common/paginate {:records 1330 :per-page 10 :max-pages 7 :current aidx :biased :left}))
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

(re-frame/reg-event-db
 ::load-kural-panel
 (fn-traced [db [_ active-panel]]
   (assoc db :active-panel active-panel)))
