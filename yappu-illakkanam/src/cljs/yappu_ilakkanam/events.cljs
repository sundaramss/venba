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

(defn- init-load [kid path]
   {:method :get
    :uri  (str "/" path "/init.json")
    :response-format (ajax/json-response-format {:keywords? true})
    :on-success [:init-data kid path]
    :on-failure [:init-data-not-received path]})

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
              (let [{fd :s init :in} (get-in context [:effects :db])
                    [_ path page padal] (get-in context [:coeffects :event])
                    httpeffects (map-indexed kural-filter-effect fd)
                    newhttpeffects (if (empty? init)
                                    (concat [(init-load nil padal)] httpeffects)
                                    httpeffects)]
               (if (not-empty newhttpeffects)
                   (assoc-in context [:effects :http-xhrio] newhttpeffects)
                   context)))))

(defn- get-fetchdata [id coll perpage]
 (let [kdata (take perpage (drop (* (dec id) perpage) coll))]
  (reduce (fn [r x] (conj r {:id x :padal nil})) [] kdata)))

(re-frame/reg-event-db
  :kural-filter-received
  [kural-fetch]
  (fn-traced [db [_ idx id res]]
    (let [perpage (get-in db [:in :perpage])
          page (:searchPage db)
          data (reduce #(conj %1 (js/parseInt %2 perpage)) (sorted-set) res)
          updatedb (update-in db [:s idx :frecords] (fn [_] data))
          allloaded (every? #(false? (nil? (% :frecords))) (updatedb :s))
          filters (when allloaded (apply intersection (map :frecords (updatedb :s))))
          pagination (when  allloaded (common/paginate {:records (count filters) :per-page perpage :max-pages 7 :current page :biased :left}))
          fetchdata (if (not-empty filters) (get-fetchdata page filters perpage) [])]
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
 (fn-traced [db [_ path page padal]]
  (let [spath (re-seq #"((\d+)/([^/]+))" path)
        search (reduce
                (fn [r x] (let [[p q] (subvec x 2)] (conj r {:id (js/parseInt p) :opt q :frecords nil})))
                [] spath)]
   (if (empty? search)
     (assoc db :load-path padal "/1")
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

(def init-interceptor
 (re-frame.core/->interceptor
  :id :init-intercept
  :after (fn [context]
          (let [[_ aidx path] (get-in context [:coeffects :event])]
           (if (some? aidx) (assoc-in context [:effects :dispatch] [::load-range-kural aidx path])
            context)))))

(re-frame/reg-event-db
  :init-data
  [init-interceptor]
  (fn-traced [db [_ id path res]]
    (assoc db :in res)))

(re-frame/reg-event-db
  :init-data-not-received
  (fn-traced [db [_ path res]]
    (assoc db :in {})))

(defn- prepare-fetchdata [sidx perpage]
 (let [krange  (let [x (+ (* sidx perpage) 1)] (range x (+ x perpage)))]
  (reduce (fn [r x] (conj r (assoc {} :id x :padal nil))) [] krange)))

(re-frame/reg-event-db
 ::load-range-kural
 [kural-fetch]
 (fn-traced [db [_ aidx path]]
  (let [ sidx (dec aidx) {padalkal :padalkal perpage :perpage} (:in db)
         newdb (clear-search-data db)
         fetchdata (prepare-fetchdata sidx perpage)
         pagination (common/paginate {:records padalkal :per-page perpage :max-pages 7 :current aidx :biased :left})]
   (assoc newdb
         :kural-adhikaram aidx
         :fetchdata fetchdata
         :pagination pagination
         :active-panel :kural-panel))))

(re-frame/reg-event-fx
 ::init-load-config
 (fn-traced [cofx [_ aidx path]]
  (let [init (get-in cofx [:db :in])]
   (if (empty? init) {:http-xhrio (init-load aidx path)}
    {:dispatch [::load-range-kural aidx  path]}))))

(re-frame/reg-event-db
 ::set-selected-opt
 (fn-traced [db [_ id value]]
   (let [idx (common/getOptIdx db id)
         search (:s db)
         padal (get-in db [:in :padal])
         nsearch (if (= value "0")
                     (remove #(= id (:id %)) search)   ;(reduce conj (subvec search 0 idx) (subvec search (inc idx)))
                     (if (nil? idx)
                        (conj search {:id id :opt value})
                        (update-in search [idx :opt] (fn [_] value))))
         path (reduce (fn [r seer](str r "/" (:id seer) "/" (:opt seer))) "" nsearch)]
      (if (empty? path) (assoc db :load-path (str "/" padal "/1")) (assoc db :load-path (str "/" padal "/s" path "?p=1"))))))

(re-frame/reg-event-db
 ::load-kural-panel
 (fn-traced [db [_ active-panel]]
   (assoc db :active-panel active-panel)))
