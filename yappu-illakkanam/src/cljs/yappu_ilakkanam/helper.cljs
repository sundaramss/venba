(ns yappu-ilakkanam.helper
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.subs :as subs]
   [yappu-ilakkanam.routes :refer [set-hash! set-token!]]))


(def asai {:NE "நே" :NI "நி"})

(def seerkal {1 [:NE] 2 [:NI] 3 [:NE :NE] 4 [:NI :NE]
              5 [:NE :NE] 6 [:NI :NE] 7 [:NE :NI] 8 [:NI :NI]
              9  [:NE :NE :NE] 10 [:NI :NE :NE] 11 [:NE :NI :NE] 12 [:NI :NI :NE]
              13 [:NE :NE :NI] 14 [:NI :NE :NI] 15 [:NI :NI :NI] 16 [:NE :NI :NI]
              17 [:NE :NE :NE :NE] 18 [:NE :NE :NE :NI] 19 [:NE :NE :NI :NE]  20 [:NE :NE :NI :NI]
              21 [:NI :NE :NE :NE] 22 [:NI :NE :NE :NI] 23 [:NI :NE :NI :NE]  24 [:NI :NE :NI :NI]
              25 [:NE :NI :NE :NE] 26 [:NE :NI :NE :NI] 27 [:NE :NI :NI :NE]  28 [:NE :NI :NI :NI]
              29 [:NI :NI :NE :NE] 30 [:NI :NI :NE :NI] 31 [:NI :NI :NI :NE]  32 [:NI :NI :NI :NI]})

(defn- prepare-path [paths]
  (reduce #(str %1 "/" (:id %2) "/" (:opt %2)) "" paths))


(defn etru-seer
  ([asaikal isKey?] (etru-seer (map name asaikal)))
  ([asaikal]
   (let [asaiJoin #(apply str %)]
      (condp contains? (asaiJoin (take 2 asaikal))
       #{"NE"} "நாள்"
       #{"NI"} "மலர்"
       #{"NENE"} "காசு"
       #{"NINE"} "பிறப்பு"))))

(defn asai-seer
  ([asaikal isKey?] (asai-seer (map name asaikal)))
  ([asaikal]
   (if (empty? asaikal) ""
     (let [asaiJoin #(apply str %)
           mudhal (condp contains? (asaiJoin (take 2 asaikal))
                   #{"NENE"} "தேமா"
                   #{"NINE"} "புளிமா"
                   #{"NINI"} "கருவிள"
                   #{"NENI"} "கூவிள")
           mudivuAsai  (last asaikal)
           mudivu (condp = (count asaikal)
                   1 ""
                   2 :>> (fn [_] (when (= "NI" mudivuAsai) "ம்"))
                   3 :>> (fn [_] (condp = mudivuAsai
                                       "NE" "ங்காய்"
                                       "NI" "ங்கனி"))
                   4 :>> (fn [_] (condp contains? (asaiJoin (take-last 2 asaikal))
                                   #{"NENE"} "ந்தண்பூ"
                                   #{"NINE"} "நறும்பூ"
                                   #{"NINI"} "நறுநிழல்"
                                   #{"NENI"} "ந்தண்ணிழல்")))]
         (str mudhal mudivu)))))

(defn get-url-pageno
 ([pageno] (get-url-pageno pageno nil))
 ([pageno pagetype]
  (let [{activepage :page padal :padal paths :paths} @(re-frame/subscribe [::subs/active-page])]
    (if (or (= activepage :pa) (= pagetype :pa)) (str "#/" padal "/" pageno)
      (let [newpath (reduce #(str %1 "/" (:id %2) "/" (:opt %2)) "" paths)]
         (str "#/" padal "/s" newpath "?p=" pageno))))))

(defn get-add-filter-url [id opt]
    (let [{padal :padal paths :paths} @(re-frame/subscribe [::subs/active-page])
          isPathExists (nil? (some #(if (= id (:id %)) %) paths))
          newpath {:id id :opt opt}
          newpaths (if isPathExists
                     (conj paths newpath)
                     (map #(if (= id (:id %)) newpath %) paths))
          path (prepare-path newpaths)]
       (str "#/" padal "/s" path "?p=1")))

(defn get-remove-filter-url [id]
    (let [{padal :padal paths :paths} @(re-frame/subscribe [::subs/active-page])
          path (prepare-path (filter #(not= id (:id %)) paths))
          newpath (str "#/" padal "/s" path "?p=1")]
       (set-token! (if (not-empty path) newpath
                       (str "#/" padal "/1")))))

(defn display-value [coll]
   (let [isEruthi @(re-frame/subscribe [::subs/is-eruthi-seer])]
     (if isEruthi (etru-seer coll true) (asai-seer coll true))))

(defn do-filter []
  (let [{selseer :selseer id :selseerid} @(re-frame/subscribe [::subs/get-selected])
        opt (apply str (map name selseer))
        newpath (get-add-filter-url id opt)]
       (set-token! newpath)))

(defn get-seleted-opts []
   (let [{paths :paths} @(re-frame/subscribe [::subs/active-page])
          eruthiSeer @(re-frame/subscribe [::subs/get-eruthi-seer])
          tagFn (fn [{id :id opt :opt}]
                 (let [optarr (re-seq #".{2}" opt)
                       value (if (= eruthiSeer id) (etru-seer optarr) (asai-seer optarr))]
                   {:id id :value value}))]
       (map tagFn paths)))

(defn get-pagination-list []
   (let [pagination @(re-frame/subscribe [::subs/get-pagination])
         pageno @(re-frame/subscribe [::subs/get-pageno])]
      (map (fn [id]
              {:pageno id
               :is-current? (= id pageno)
               :href (get-url-pageno id)})
         pagination)))

(defn- get-value []
 (let [eruthiSeer @(re-frame/subscribe [::subs/get-eruthi-seer])
       colsLen @(re-frame/subscribe [::subs/get-total-cols])]
   (fn [ridx cols]
      (map-indexed (fn [cidx col]
                     (let [isEruthi (= (+ (* ridx colsLen) cidx 1) eruthiSeer)]
                        (if isEruthi (etru-seer col) (asai-seer col))))
        cols))))

(def asai-ta {"NI" "நி" "NE" "நே"})

(defn- get-asai [col]
   (map (fn[xcol] (vec (map #(get asai-ta %) xcol))) col))

(defn- prepare-breadcrumbs [pal iyal adhikaram]
    (let [{palno :palno iyalno :iyalno adhikaramno :adhikaramno} @(re-frame/subscribe [::subs/get-adhikaram-for [pal iyal adhikaram]])
           palurl (get-url-pageno palno :pa)
           iyalurl (get-url-pageno iyalno :pa)
           adhikaramurl (get-url-pageno adhikaramno :pa)]
       (conj []
            {:id palno :url palurl :name pal}
            {:id iyalno :url iyalurl :name iyal}
            {:id adhikaramno :url adhikaramurl :name adhikaram})))


(defn get-padals []
  (let [padallist @(re-frame/subscribe [::subs/get-padallist])]
     (map (fn [[pno padal]]
             (let [{sp :sp aplist :ap pal :pal iyal :iyal adhikaram :adhikaram} padal
                    aps (map-indexed (get-value) aplist)
                    asais (map get-asai aplist)
                    breadcrumbs (prepare-breadcrumbs pal iyal adhikaram)
                    lines (map #(apply map vector %) (map vector sp asais aps))]
                  {:id pno :lines lines :breadcrumbs breadcrumbs}))
        padallist)))
