(ns yappu-ilakkanam.events
  (:require
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.db :as db]
   [yappu-ilakkanam.common :as common]
   [clojure.set :refer [intersection]]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
   [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]))

(defn- clear-range-data [db]
  db)

(defn- clear-search-data [db]
  (-> db
    (dissoc :searchPage :filters)
    (assoc :s [])))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
   db/default-db))


(re-frame/reg-event-db
 ::set-active-panel
 (fn-traced [db [_ active-panel]]
   (assoc db :active-panel active-panel)))

(defn- kural-req-effect [idx {kid :id}]
   {:method :get
    :uri  (str "/kural/extra/" kid ".json")
    :response-format (ajax/json-response-format {:keywords? true})
    :on-success [:kural-received idx kid]
    :on-failure [:kural-not-received idx kid]})


(defn- kural-filter-effect [idx {sid :id option :opt records :frecords}]
   {:method :get
    :uri  (str "/kural/s/" option "/" sid ".json")
    :response-format (ajax/json-response-format {:keywords? true})
    :on-success [:kural-filter-received idx sid]
    :on-failure [:kural-filter-not-received idx sid]})

(def kural-fetch
   (re-frame/->interceptor
     :id :kural-fetch
     :after (fn [context]
              (let [fd (get-in context [:effects :db :fetchdata])
                    httpeffects (map-indexed kural-req-effect fd)]
               (if (not-empty httpeffects)
                   (assoc-in context [:effects :http-xhrio] httpeffects)
                   context)))))
(def kural-filter-fetch
   (re-frame/->interceptor
     :id :kural-filter-fetch
     :after (fn [context]
              (let [fd (get-in context [:effects :db :s])
                    httpeffects (map-indexed kural-filter-effect fd)]
               (if (not-empty httpeffects)
                   (assoc-in context [:effects :http-xhrio] httpeffects)
                   context)))))

(defn- get-fetchdata [id coll]
 (let [kdata (take 10 (drop (* (dec id) 10) coll))]
  (reduce (fn [r x] (conj r {:id x :padal nil})) [] kdata)))

(re-frame/reg-event-db
  :kural-filter-received
  [kural-fetch]
  (fn-traced [db [_ idx id res]]
    (let [data (reduce #(conj %1 (js/parseInt %2 10)) (sorted-set) res)
          page (:searchPage db)
          updatedb (update-in db [:s idx :frecords] (fn [_] data))
          allloaded (every? #(false? (nil? (% :frecords))) (updatedb :s))
          filters (when allloaded (apply intersection (map :frecords (updatedb :s))))
          pagination (when  allloaded (common/paginate {:records (count filters) :per-page 10 :max-pages 7 :current page :biased :left}))
          fetchdata (if (not-empty filters) (get-fetchdata page filters) [])]
      (if allloaded
       (assoc updatedb :filters filters :pagination pagination :fetchdata fetchdata)
       updatedb))))

(re-frame/reg-event-db
  :kural-filter-not-received
  (fn [db [_ id res]]
    (prn "Failure:" id res)
    db))

(re-frame/reg-event-db
 ::search-kural
 [kural-filter-fetch]
 (fn-traced [db [_ path page]]
  (let [spath (re-seq #"((\d+)/([^/]+))" path)
        search (reduce
                (fn [r x] (let [[p q] (subvec x 2)] (conj r {:id (js/parseInt p) :opt q :frecords nil})))
                [] spath)]
   (if (empty? search)
     (assoc db :load-path "kural/1")
     (-> db
      (assoc :s search :searchPage (js/parseInt page))
      (dissoc :kural-adhikaram :ar :fetchdata)
      (assoc :active-panel :kural-panel))))))

(re-frame/reg-event-db
  :kural-received
  (fn-traced [db [_ idx id res]]
    (update-in db [:fetchdata idx :padal] (fn [_] res))))

(re-frame/reg-event-db
  :kural-not-received
  (fn [db [_ id res]]
    (prn "Failure:" id res)
    db))

(defn- prepare-fetchdata [sidx]
 (let [krange  (let [x (+ (* sidx 10) 1)] (range x (+ x 10)))]
  (reduce (fn [r x] (conj r (assoc {} :id x :padal nil))) [] krange)))

(re-frame/reg-event-db
 ::load-range-kural
 [kural-fetch]
 (fn-traced [db [_ aidx]]
  (let [ sidx (dec aidx)
         newdb (clear-search-data db)
         fetchdata (prepare-fetchdata sidx)
         pagination (common/paginate {:records 1330 :per-page 10 :max-pages 7 :current aidx :biased :left})]
    (assoc newdb
     :kural-adhikaram aidx
     :fetchdata fetchdata
     :pagination pagination 
     :active-panel :kural-panel))))

(re-frame/reg-event-db
 ::set-selected-opt
 (fn-traced [db [_ id value]]
   (let [idx (common/getOptIdx db id)
         search (:s db)
         nsearch (if (= value "0")
                     (remove #(= id (:id %)) search)   ;(reduce conj (subvec search 0 idx) (subvec search (inc idx)))
                     (if (nil? idx)
                        (conj search {:id id :opt value})
                        (update-in search [idx :opt] (fn [_] value))))
         path (reduce (fn [r seer](str r "/" (:id seer) "/" (:opt seer))) "" nsearch)]
      (if (empty? path) (assoc db :load-path "/kural/1") (assoc db :load-path (str "/kural/s" path "?p=1"))))))

(re-frame/reg-event-db
 ::load-kural-panel
 (fn-traced [db [_ active-panel]]
   (assoc db :active-panel active-panel)))
