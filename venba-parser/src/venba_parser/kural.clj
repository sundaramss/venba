(ns venba-parser.kural
  (:require [venba-parser.common :as common]
            [venba-parser.yappu :as yappu]
            [venba-parser.pa :as pa]
            [clojure.spec.alpha :as s]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [clojure.set :as cset]
            [clojure.core.async :refer [mult tap merge chan take! put! <!! >!! <! >! onto-chan pipeline pipeline-async pipeline-blocking] :as async]
            [clojure.java.io :as io]))

(defn parse-kural-record [kurlrec]
 (let [columns (map str/trim (str/split kurlrec #","))
       kuralArr (partition-all 4 (str/split (last columns) #" +"))
       cntx (zipmap [:venba :padal :ve :ave :pal :iyal :adhikaram] (conj (drop-last columns) kuralArr "kural"))] cntx))

(defn- kural-folder [{ave :ave pal :pal iyal :iyal adhikaram :adhikaram}]
  [pal iyal adhikaram ave])

(defn- kural-text [{padal :padal}]
 (str/join "\n" (map #(str/join " " %) padal)))

(defn- kural-txt-file [{ave :ave pal :pal iyal :iyal adhikaram :adhikaram}]
  (str pal "/" iyal "/" adhikaram "/" ave ".txt"))

(defn- kural-json-file [{ve :ve}]
  (str "target/" ve ".json"))

(defn- kural-extra-json [d]
  (common/clj-to-json (dissoc d :al :ave :ve)))

(defn- kural-extra-json-file [{ve :ve}]
  (str "extra/" ve ".json"))

;(defn prepare-kural [kuralrec]
; (->> kuralrec
;  (parse-kural-record)
;  ((juxt (comp common/make-dir kural-folder) identity))
;  (last)
;  ((juxt (juxt kural-txt-file kural-text)))
;  (map #(apply write-venba %))))

(defn prepare-json-kural [kuralrec]
 (->> kuralrec
  (parse-kural-record)
  ((juxt (juxt kural-json-file common/clj-to-json)))))

(defn write-kural [[data]]
 (apply common/write-venba data))


(defn kural-data-setup [input-file] ""
  (with-open [rdr (io/reader input-file)]
   (let [lines (line-seq rdr)]
    (dorun
     (map #(write-kural %)
          (pmap prepare-json-kural lines))))))

(defn kural-alakidu [lseer? seerkal]
   (let [ kuralasaikal (map yappu/asaiPerkal seerkal)
           e (if lseer? last identity)
           f (if lseer? drop-last identity)
           etruseer (e kuralasaikal)
           seers (f kuralasaikal)
           a (map yappu/asai-seer seers)
           b (yappu/etru-seer etruseer)]
     (conj (vec a) b)))

(defn- update-field [result k v]
  (update result k #(concat % [v])))

(defn kural-extra [result seerkal]
  (let [lseer? (= 3 (count seerkal))
        sp (map yappu/sol-asai-piri seerkal)
        op (map #(-> % (yappu/tamil-ezhuthu-split) (yappu/osaikal-piri) (yappu/asai-peeri)) seerkal)
        ;al (kural-alakidu lseer? seerkal)
        ap (map yappu/asaiPerkal seerkal)]
     (-> result
       (update-field :sp sp)
       ;(update-field :op op)
       (update-field :ap ap))))
       ;(update-field :al al))))

(defn padal [{p :padal ave :ave ve :ve pal :pal iyal :iyal adhikaram :adhikaram}]
    (reduce kural-extra {:ave ave :ve ve :pal pal :iyal iyal :adhikaram adhikaram} p))

(defn prepare-json-kural-extra [kuralrec]
 (->> kuralrec
  (parse-kural-record)
  (padal)
  ;(kural-extra)
  ((juxt (juxt kural-extra-json-file kural-extra-json)))))

(defn kural-extra-data-setup [input-file] ""
  (with-open [rdr (io/reader input-file)]
   (let [lines (line-seq rdr)]
    (dorun
     (map #(write-kural %)
          (map prepare-json-kural-extra lines))))))

(defn x1 [y] (reduce (fn [r x] (concat r x) ) [] y))
(defn x2 [y] (map #(map name %) y))
(defn x3 [y] (drop-last (map #(apply str %) y)))
(defn x31 [y] (map vector y (drop 1 (range 10))))
(defn x4
     ([i r x] (update-in r x #(conj (or % []) i))))

(defn cookup-search-data [r k]
 (let [p (->> k (parse-kural-record) (padal))
       d (:ap p)
       i (:ve p)]
  (reduce (partial x4 i) r ((comp x31 x3 x2 x1) d))))

(defn cookup-search-7data [r k]
 (let [p (->> k (parse-kural-record) (padal))
       d (:ap p)
       i (:ve p)
       eruthi (reduce str "" (map name (last (last d))))]
   (x4 i r [eruthi 7])))

(defn countV [vm]
   (reduce-kv (fn [r k v] (set (concat r (map read-string v)))) [] vm))

(defn cookup-search-data-val [r]
   (reduce-kv (fn [m k v] (assoc m k (sort (countV v)))) {} r))
  ;(reduce-kv (fn [m k vm] (prn (str "***" k "***")) (assoc m k vm)) {} fdata)))

(defn kural-search-data [input-file] ""
  (with-open [rdr (io/reader input-file)]
   (let [lines (line-seq rdr) r {}]
     (common/write-venba "test.json" (common/clj-to-json (reduce cookup-search-data r lines))))))

(defn kural-search-data-val [input-file] ""
  (with-open [rdr (io/reader input-file)]
   (let [lines (line-seq rdr) r {}]
     (common/write-venba "test-val.json" (-> (reduce cookup-search-data r lines)
                                          (cookup-search-data-val)
                                          (common/clj-to-json))))))
(defn- write-search-result [asai result]
  (common/make-dir (str "extra" "/" asai "/1.json"))
  (doseq [[seerid listValue] result]
    (common/write-venba  (str "extra/" asai "/" seerid ".json") (common/clj-to-json listValue))))

(defn kural-write-search-data-val [input-file] ""
  (with-open [rdr (io/reader input-file)]
   (let [lines (line-seq rdr) r {}
         result (reduce cookup-search-data r lines)]
     (doseq [[key value] result]
         (write-search-result key value)))))

(defn kural-write-search-7data-val [input-file] ""
  (with-open [rdr (io/reader input-file)]
   (let [lines (line-seq rdr) r {}
         result (reduce cookup-search-7data r lines)]
     (doseq [[key value] result]
         (write-search-result key value)))))

(s/def ::kural-csv-rec (s/* (s/cat :ve string?
                               :ave string?
                               :pal string?
                               :iyal string?
                               :adhikaram string?
                               :padal (s/* string?))))


(defn kural-rec [arr]
  (let [val (s/conform ::kural-csv-rec arr)]
   (if (= val ::s/invalid) [] (first val))))


(defn kural-write [data]
  (let [wfile (str "./extra/" (:ve data) ".json")
        recs (assoc data :sp (partition-all 4 (:sp data))
                         :ap (partition-all 4 (:ap data))
                         :op (partition-all 4 (:op data)))
        json-data (common/clj-to-json (dissoc recs :ve))]
    (spit wfile json-data)
    (:ve data)))

;(defn kural-validate1 [result data]
;   (let [[seers :seers thalai :thalai ve :ve] data
;          seerUpdateResult (if (nil? seers) (update-in result [:seers] conj ve) result)
;          thalaiUpdateResult (if (nil? thalai) (update-in seerUpdateResult [:thalai] conj ve) thalaiUpdateResult)
;     thalaiUpdateResult)))

(defn kural-validate12 [result data]
   (let [seers (:seers data)
         thalai (:thalai data)
         ve (:ve data)
         seerUpdateResult (if (nil? seers) (update-in result [:seers-invalid] conj ve) result)
         thalaiUpdateResult (if (nil? thalai) (update-in result [:thalais-invalid] conj ve) seerUpdateResult)]
    (when (nil? thalai) (println (pa/thalais-explain-str result)))
    thalaiUpdateResult))

(defn kural-data [filepath]
 (with-open [reader (io/reader filepath)]
    (let [data (csv/read-csv reader)]
      (mapv
         #(->> %
           kural-rec
           pa/padal)
        data))))

(defn kural-data-validate [filepath]
 (with-open [reader (io/reader filepath)]
    (let [data (csv/read-csv reader)]
      (mapv
         #(->> %
           kural-rec
           padal)
        data))))

(defn kurals-write [filepath]
    (mapv #(kural-write %) (kural-data filepath)))

(defn padal [recs]
 (let [p (pa/padal recs)
       thalaiExists (:thalai p)
       ve (:ve recs)
       isAytha (pa/isAytha? (first (:padal recs)))]
   (if (and (nil? thalaiExists) isAytha) (pa/padal recs true) p)))

(defn kurals-validate [filepath]
   (println
    (reduce #(kural-validate12 %1 %2) {:seers-invalid [] :thalais-invalid []} (kural-data-validate filepath))))

; (def k1021 "1021,1,பொருட்பால்,குடியியல்,குடிசெயல்வகை, கருமம் செயஒருவன் கைதூவேன் என்னும்  பெருமையில் பீடுஉடையது இல்")

;  ["1021","1","பொருட்பால்","குடியியல்","குடிசெயல்வகை" "கருமம்" "செயஒருவன்" "கைதூவேன்" "என்னும்"  "பெருமையில்" "பீடுஉடையது" "இல்"])
; "22,2,அறத்துப்பால்,பாயிரம் ,நீத்தார் பெருமை, துறந்தார் பெருமை துணைக்கூறின் வையத்து இறந்தாரை எண்ணிக்கொண் டற்று"))
(->>
;   ["1021","1","பொருட்பால்","குடியியல்","குடிசெயல்வகை" "கருமம் செயஒருவன் கைதூவேன் என்னும்  பெருமையில் பீடுடைய தில்"])
   (str/split
     "1014,4,பொருட்பால்,குடியியல்,நாணுடைமை, அணியன்றோ நாணுடைமை சான்றோர்க் கஃதின்றேல் பிணியன்றோ பீடு நடை"
     ;"291,1,அறத்துப்பால்,துறவறவியல்,வாய்மை, வாய்மை எனப்படுவ தியாதெனின் யாதொன்றந் தீமை யிலாத சொலல்"
    #",")
  (kural-rec)
  (padal))
  ;(pa/thalais-explain-str))
  ;(pa/padal)
  ;(:seers))

(defn kural-rec-chan [arr]
  (let [val (s/conform ::kural-csv-rec arr)]
    (if (= val ::s/invalid) [] (first val))))

(defn kural-pa-chan [arr]
  (dissoc (padal arr) :seers :thalai))

(defn kural-write-chan [data]
    (kural-write data))

(defn calculate-freq [result arr pano]
  (let [number-asai (map-indexed (fn [i v] [(inc i) v]) arr)]
   (reduce (fn [r v]
             (let [f (first v) l (last v)]
               (update-in r [l] (fn [x y z] (update-in x [y] conj z)) f pano)))
     result number-asai)))

(println (calculate-freq {[:NI] {1 '(43)}, [:NE :NI] {2 '(43)}} [[:NI] [:NE :NE]] 100))

(defn kural-calculate [frequencyMap]
   (fn [data]
    (let [ve (:ve data) ap (:ap data)]
     (reset! frequencyMap (calculate-freq @frequencyMap ap ve))
     true)))

(defn- write-frequency-result [asai result]
 (let [mainKey (apply str (map name asai))]
  (doseq [[seerid listValue] result]
   (let [file-name (str "s/" mainKey "/" seerid ".json")]
    (io/make-parents file-name)
    (common/write-venba file-name (common/clj-to-json (reverse listValue)))))))

(defn write-frequency [frequencyMap]
  (doseq [[k v] frequencyMap]
     (write-frequency-result k v)))

(defn kurals-write-parallel [filepath]
    (let [raw-in (let [c (chan 1024)]
                   (async/go
                    (with-open [reader (io/reader filepath)]
                      ;(doseq [data (line-seq reader)])
                      (doseq [data (csv/read-csv reader)]
                       (put! c data)))
                    (async/close! c))
                   c)
           frequencyMap (atom {})
           k-calculate (kural-calculate frequencyMap)
           kural-out-in (chan 1024)
           pa-out-in (chan 1024)
           m (mult pa-out-in)
           write-tap (chan 1024)
           calc-tap (chan 1024)
           out1 (chan 1024)
           out2 (chan 1024)
           out (merge [out1 out2] 1024)]
     (tap m write-tap)
     (tap m calc-tap)
     ;(onto-chan raw-in (range 10))
     ;(pipeline-blocking 1 out (map identity) raw-in)
     (pipeline-blocking 16 kural-out-in (map kural-rec-chan) raw-in)
     (pipeline-blocking 25 pa-out-in (map kural-pa-chan) kural-out-in)
     ;(pipeline-blocking 32 out1 (map kural-write-chan) pa-out-in)
     (pipeline-blocking 32 out1 (map kural-write-chan) write-tap)
     (pipeline-blocking 5 out2 (map k-calculate) calc-tap)
      ;(pipeline-async 2 out kural-write-chan pa-in)
     (<!! (async/into [] out))
     ;(println (map keys (vals @frequencyMap)))))
     (write-frequency @frequencyMap)))


(pa/isAytha? "அஃதிலார்க்கு")
