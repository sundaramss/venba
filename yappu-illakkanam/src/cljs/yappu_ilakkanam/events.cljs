(ns yappu-ilakkanam.events
  (:require
   [clojure.string :refer [split]]
   [clojure.set :refer [intersection]]
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.db :as db]
   [day8.re-frame.async-flow-fx]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
   [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]))

(def seerkal {1 [:NE] 2 [:NI] 3 [:NE :NE] 4 [:NI :NE]
              5 [:NE :NE] 6 [:NI :NE] 7 [:NE :NI] 8 [:NI :NI]
              9  [:NE :NE :NE] 10 [:NI :NE :NE] 11 [:NE :NI :NE] 12 [:NI :NI :NE]
              13 [:NE :NE :NI] 14 [:NI :NE :NI] 15 [:NI :NI :NI] 16 [:NE :NI :NI]
              17 [:NE :NE :NE :NE] 18 [:NE :NE :NE :NI] 19 [:NE :NE :NI :NE]  20 [:NE :NE :NI :NI]
              21 [:NI :NE :NE :NE] 22 [:NI :NE :NE :NI] 23 [:NI :NE :NI :NE]  24 [:NI :NE :NI :NI]
              25 [:NE :NI :NE :NE] 26 [:NE :NI :NE :NI] 27 [:NE :NI :NI :NE]  28 [:NE :NI :NI :NI]
              29 [:NI :NI :NE :NE] 30 [:NI :NI :NE :NI] 31 [:NI :NI :NI :NE]  32 [:NI :NI :NI :NI]})

(defn- list-to-map [coll]
   (if (nil? coll) {}
     (into (sorted-map) (persistent! (reduce
                                       #(assoc! %1 %2 nil)
                                       (transient {})
                                       coll)))))
(defn- map-opt [myfn coll]
 (persistent! (reduce  myfn (transient []) coll)))

(defn- set-path-result [path id res]
   (if (= id (:id path)) (assoc path :frecords (apply sorted-set res))
     path))

(defn- refinedCID [cid totalPages]
   (if (< cid 1) 1
       (if (<= cid totalPages) cid totalPages)))

(defn- prepare-filters [paths]
  (let [filters (apply intersection (map :frecords paths))]
     (vec (partition-all 10 filters))))

(defn- preparePagination [cid end]
  (let [current (refinedCID cid end)]
    (cond
     (or (< end current) (< current 1)) []
     (<= (dec end) 4) (vec (range 1 (inc end)))
     (>= 3 current) [1 2 3 4 nil end]
     (< (- end 3) current) (into [1 nil] (range (- end 3) (inc end)))
     :else [1 nil (dec current) current (inc current) nil end])))

(defn- http-config [uri success failure]
   {:method :get
    :uri uri
    :response-format (ajax/json-response-format {:keywords? true})
    :on-success success
    :on-failure failure})

(defn- fetch-config [padal]
   (let [uri (str "/" padal "/config.json")]
     (http-config uri [::success-config] [::failure-config])))

(defn- fetch-resource [padal id]
   (let [uri (str "/" padal "/extra/" id ".json")]
     (http-config uri [::success-resource id] [::failure-resource id])))

(defn- fetch-search-config [padal path]
   (let [{opt :opt id :id} path
         uri (str "/" padal "/s/" (str opt "/" id) ".json")]
     (http-config uri [::success-search id] [::failure-search id])))

(defn- new-padal-load []
   {:first-dispatch [::load-config]
    :rules [{:when :seen? :events ::success-config  :dispatch-n [[::prepare-page-padallist] [::prepare-pagination] [::fetch-padals]]}
            {:when :seen? :events ::failure-config :halt? true}]})

(defn- new-search [isConfigExist]
  (let [first (if isConfigExist [::search-load-config] [::load-config])]
   {:first-dispatch first
    :rules [{:when :seen? :events ::success-search :dispatch-n [[::prepare-s-pagination] [::fetch-padals]]}
            {:when :seen? :events ::success-config :dispatch [::search-load-config]}
            {:when :seen? :events ::failure-config :halt? true}
            {:when :seen? :events ::failure-config :halt? true}]}))

(defn- prepare-seers-list [db selseerid]
  (let [prevseerid (get-in db [:selected :selseerid])]
     (if (= selseerid prevseerid) db
       (let [seers (get-in db [:config selseerid])
             seerlist  (map #(conj [] % (get seerkal %)) seers)
             prevseer (get-in db [:selected :selseer])
             defaultValue (get seerkal (first seers))]
        (-> db
         (assoc-in [:selected :seerlist] seerlist)
         (assoc-in [:selected :selseer] (or prevseer defaultValue)))))))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
   db/default-db))

(re-frame/reg-event-fx
 ::load-config
 (fn-traced [{:keys [db]} [_]]
     (let [padal (get-in db [:active-page :padal] 'kural')]
        {:http-xhrio (fetch-config padal)})))

(re-frame/reg-event-fx
 ::search-load-config
 (fn-traced [{:keys [db]} [_]]
    (let [{padal :padal paths :paths} (get-in db [:active-page] {})
           http-apis (map-opt #(conj! %1 (fetch-search-config padal %2)) (seq paths))]
      {:http-xhrio http-apis
       :db (assoc db :padallist {})})))

(re-frame/reg-event-db
 ::success-config
 (fn-traced [db [_ res]]
   (-> db
     (assoc :config res)
     (prepare-seers-list :asaiSeer)
     (assoc-in [:selected :selseerid] 1))))

(re-frame/reg-event-db
 ::success-resource
 (fn-traced [db [_ id res]]
        (assoc-in db [:padallist id] res)))

(re-frame/reg-event-db
 ::success-search
 (fn-traced [db [_ id res]]
   (let [paths (get-in db [:active-page :paths])
         newpaths (map-opt #(conj! %1 (set-path-result %2 id res)) paths)
         filters (prepare-filters newpaths)
         size (count filters)
         pageid (get-in db [:active-page :id] 0)]
       (-> db
        (update-in [:active-page] assoc :paths newpaths :filters filters)
        (assoc :padallist (list-to-map (get filters (mod (dec pageid) size) [])))))))

(re-frame/reg-event-db
  ::prepare-page-padallist
  (fn-traced [db [_]]
    (let [padal (get-in db [:active-page :padal] 'kural')
          id (get-in db [:active-page :id] 1)
          {a :a z :z} (get-in db [:config :pages (keyword (str id))] {:a 1 :z 10})
          padallist (range a (+ z 1))]
       (assoc db :padallist (list-to-map padallist)))))

(re-frame/reg-event-db
  ::prepare-pagination
  (fn-traced [db [_]]
    (let [padal (get-in db [:active-page :padal] 'kural')
          id (get-in db [:active-page :id] 1)
          totalPages (get-in db [:config :totalpages] 0)
          pagination (preparePagination id totalPages)]
      (assoc db :pagination pagination))))

(re-frame/reg-event-db
  ::prepare-s-pagination
  (fn-traced [db [_]]
      (let [totalPages (count (get-in db [:active-page :filters] []))
            id (get-in db [:active-page :id])
            pagination (preparePagination id totalPages)]
        (assoc db :pagination pagination))))

(re-frame/reg-event-fx
 ::fetch-padals
 (fn-traced [{:keys [db]} [_]]
    (let [padal (get-in db [:active-page :padal] 'kural')
          padallist (keys (:padallist db))
          pages (map-opt #(conj! %1 (fetch-resource padal %2)) padallist)]
        (if (not-empty padallist) {:http-xhrio pages} {}))))

(re-frame/reg-event-fx
 ::set-active-page
 (fn-traced [{:keys [db]} [_ {:keys [page padal id paths]}]]
    (let [newdb (assoc db :active-page {:page page :padal padal :id id :paths paths})]
      (case page
         :pa {:db newdb
              :async-flow (new-padal-load)}
         :pasearch (let [isConfigExist (not-empty (:config newdb))]
                     {:db newdb
                      :async-flow (new-search isConfigExist)})))))

;; UI events
(re-frame/reg-event-db
  ::set-seer-id-selection
  (fn-traced [db [_ id]]
    (let [eruthiSeer (get-in db [:config :eruthiSeer])
          selseerid (if (= eruthiSeer id) :etruSeer :asaiSeer)]
      (-> db
        (prepare-seers-list selseerid)
        (assoc-in [:selected :selseerid] id)))))

(re-frame/reg-event-db
  ::set-seer-selection
  (fn-traced [db [_ value]]
    (let [varr (map keyword (split value #","))]
      (assoc-in db [:selected :selseer] varr))))
