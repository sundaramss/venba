(ns venba-parser.kural
  (:require
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [venba-parser.common :as common]
            [venba-parser.pa :as pa]
            [clojure.spec.alpha :as s]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [clojure.set :as cset]
            [clojure.core.async :refer [mult tap chan take! put! <!! >!! <! >! onto-chan pipeline pipeline-async pipeline-blocking] :as async]
            [clojure.java.io :as io]))

(s/def ::kural-csv-rec (s/* (s/cat :pno string? :ve string?
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

(defn padal [recs]
 (let [p (pa/padal recs)
       thalaiExists (:thalai p)
       ve (:ve recs)
       isAytha (pa/isAytha? (first (:padal recs)))]
   (if (and (nil? thalaiExists) isAytha) (pa/padal recs true) p)))

(defn kural-rec-chan [arr]
  (let [val (s/conform ::kural-csv-rec arr)]
    (if (= val ::s/invalid) [] (first val))))

(defn kural-pa-chan [arr]
  (dissoc (padal arr) :seers :thalai))

(defn kural-write-chan [data]
    (kural-write data))

(defn calculate-freq [dataMap arr pano]
  (let [number-asai (map-indexed (fn [i v] [(inc i) v]) arr)
        result (:freqMap dataMap)
        newFreq (reduce (fn [r v]
                         (let [f (first v) l (last v)]
                           (update-in r [l f] (fnil conj (sorted-set-by >)) pano)))
                   result number-asai)]
    (assoc dataMap :freqMap newFreq)))

(defn aggreate-title [dataMap {iyal :iyal adhikaram :adhikaram pal :pal} pno ve]
     (let [pakalMinMap (update-in dataMap [:sector :pal pal :a] (fnil min pno) pno)
           pakalMaxMap (update-in pakalMinMap [:sector :pal pal :z] (fnil max pno) pno)
           iyalkalMinMap (update-in pakalMaxMap [:sector :iyal iyal :a] (fnil min pno) pno)
           iyalkalMaxMap (update-in iyalkalMinMap [:sector :iyal iyal :z] (fnil max pno) pno)
           adhikaramkalMinMap (update-in iyalkalMaxMap [:sector :adhikaram adhikaram] (fnil min pno) pno)
           ;adhikaramkalMaxMap (update-in adhikaramkalMinMap [:sector :adhikaram adhikaram :z] (fnil max ve) ve)
           totalpages (update-in adhikaramkalMinMap [:sector :totalpages] (fnil max pno) pno)
           minpages (update-in totalpages [:sector :pages pno :a] (fnil min ve) ve)
           maxpages (update-in minpages [:sector :pages pno :z] (fnil max ve) ve)]
      maxpages))

(defn kural-calculate [dataMapRef]
   (fn [data]
    (let [{pnoStr :pno ap :ap veStr :ve } data
          pno (edn/read-string pnoStr)
          ve (edn/read-string veStr)]
     (dosync
        (alter dataMapRef calculate-freq ap ve)
        (alter dataMapRef aggreate-title data pno ve))
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

(defn write-config [config]
   (let [json-data (common/clj-to-json config)
         file-name "config.json"]
    (common/write-venba file-name json-data)))

(def k-config {:iyal {} :adhikaram {} :pal {} :eruthiSeer 7 :lineCols 4 :asaiSeer (range 5 13) :etruSeer (range 1 5)})

(defn kurals-write-parallel [filepath]
    (let [raw-in (let [c (chan 1024)]
                   (async/go
                    (with-open [reader (io/reader filepath)]
                      ;(doseq [data (line-seq reader)])
                      (doseq [data (csv/read-csv reader)]
                       (put! c data)))
                    (async/close! c))
                   c)
           dataMap (ref {:freqMap {}
                          :sector k-config})
           k-calculate (kural-calculate dataMap)
           kural-out-in (chan 1024)
           pa-out-in (chan 1024)
           m (mult pa-out-in)
           write-tap (chan 1024)
           calc-tap (chan 1024)
           out1 (chan 1024)
           out2 (chan 1024)
           out (async/merge [out1 out2] 1024)]
     (tap m write-tap)
     (tap m calc-tap)
     (pipeline-blocking 16 kural-out-in (map kural-rec-chan) raw-in)
     (pipeline-blocking 25 pa-out-in (map kural-pa-chan) kural-out-in)
     (pipeline-blocking 32 out1 (map kural-write-chan) write-tap)
     (pipeline-blocking 5 out2 (map k-calculate) calc-tap)
     (<!! (async/into [] out))
     (write-frequency (:freqMap @dataMap))
     (write-config (:sector @dataMap))))

;(pa/isAytha? "அஃதிலார்க்கு")
;(println (calculate-freq {[:NI] {1 '(43)}, [:NE :NI] {2 '(43)}} [[:NI] [:NE :NE]] 100))
(defn kural-validate12 [result data]
   (let [seers (:seers data)
         thalai (:thalai data)
         ve (:ve data)
         seerUpdateResult (if (nil? seers) (update-in result [:seers-invalid] conj ve) result)
         thalaiUpdateResult (if (nil? thalai) (update-in result [:thalais-invalid] conj ve) seerUpdateResult)]
    (when (nil? thalai) (println (pa/thalais-explain-str result)))
    thalaiUpdateResult))

(defn kural-data-validate [filepath]
 (with-open [reader (io/reader filepath)]
    (let [data (csv/read-csv reader)]
      (mapv
         #(->> %
           kural-rec
           padal)
        data))))

(defn kurals-validate [filepath]
   (println
    (reduce #(kural-validate12 %1 %2) {:seers-invalid [] :thalais-invalid []} (kural-data-validate filepath))))

(defn prime-number?
  [n]
  (println "Checking for Prime:>>" n (Math/sqrt n))
  (let [square-root  100000 ;(Math/sqrt n)
        mod-zero? (fn [res new-val]
                    (if (zero? (mod n new-val))
                      (reduced false)
                      true))]
    (reduce mod-zero? true (range 2 (inc square-root)))))

(def binary {:NE 0 :NI 1})

(defn asai-binary [arr]
   (reduce #(str %1 (get binary %2)) "" arr))

(defn b-to-n [s]
  (Integer/parseInt s 2))

(defn seer-numbering [arr]
   (reduce #(+ (* %1 10) (b-to-n (asai-binary %2))) 0 arr))

(defn full-seer-numbering [arr]
   (let [bdata (reduce #(str %1 (asai-binary %2)) "" arr)]
    (b-to-n bdata)))
(seer-numbering [[:NE :NE :NE] [:NE :NE :NE] [:NE :NE :NE] [:NE :NE :NE] [:NE :NE :NE] [:NE :NE] [:NI]])

(def asai-octal-numbers {[:NE :NE] 0 [:NI :NE] 1
                         [:NI :NI] 2 [:NE :NI] 3
                         [:NE :NE :NE] 4 [:NI :NE :NE] 5
                         [:NI :NI :NE] 6 [:NE :NI :NE] 7})

(def etru-octal-numbers {[:NE] 0 [:NI] 1 [:NE :NE] 2 [:NI :NE] 3})

(def trinary {:O 0 :K 1 :N 2})

(defn asai-trinary [arr]
   ;(print arr)
   (reduce #(str %1 (get trinary %2)) "" arr))

(defn t-to-n [s]
  ;(println (Integer/parseInt s 3))
  (Integer/parseInt s 3))

(defn tri-numbering [arr]
   ;(reduce #(+ (* %1 10) (t-to-n (asai-trinary %2))) 0 arr)
   (reduce #(str %1  (t-to-n (asai-trinary %2))) "" arr))

(defn seer-tri-numbering [arr]
   ;(reduce #(+ (* %1 (Math/pow 10 (count %2))) (tri-numbering %2)) 0 arr)
   (let [data (reduce #(str %1 (tri-numbering %2)) "" arr)]
     (println "Data>>>" (bigint data))
     data))

(println
 (seer-tri-numbering
   [[[:N] [:K :O]] [[:K :N] [:K :O]] [[:K]]]))

(defn kural-numbering [result record]
  (let [{ve :ve ap :ap op :op} record
         seernumber (seer-numbering ap)
         seertrinumber (seer-tri-numbering op)
         isprime? false; (prime-number? seernumber)
         isprimetri? false; (prime-number? seertrinumber)
         data {:pno ve
               :seernumber {:no seernumber :isprime? isprime?}
               :triseernumber {:no seertrinumber :isprime? isprimetri?}}]
     (update-in result [:simple] conj data)))

;(kural-numbering {:simple []} (assoc {} :ve 811 :ap [[:NE :NE :NE] [:NE :NE :NE] [:NE :NE :NE] [:NE :NE :NE] [:NE :NE :NE] [:NE :NE] [:NI]]))

(defn kurals-number-system [filepath]
  (let [data (reduce #(kural-numbering %1 %2) {:simple []} (kural-data-validate filepath))
        fdata
            (filter (fn [{pno :pno seerNumber :seerNumber triSeerNumber :triseernumber}]
                      (let [lastChar (last (:no triSeerNumber))
                            lastDivisible (some #(= lastChar %) [\0 \2 \4 \6 \8 \5])]
                        ;(println "~~~~~~~~~~~~" lastChar lastDivisible (:no triSeerNumber))
                        (if lastDivisible false
                           (prime-number? (bigint (:no triSeerNumber))))))
                        ;(:isprime? seerNumber)))
                       ;(:isprime? triSeerNumber)))
              (:simple data))]
     (println fdata)))
     ;(println fdata (count fdata))))
              ;(let [isseerPrime (get seerNumber :isprime? false)]
              ;  (when true (println seerNumber))
              ; isseerPrime)) data))
; (def k1021 "1021,1,பொருட்பால்,குடியியல்,குடிசெயல்வகை, கருமம் செயஒருவன் கைதூவேன் என்னும்  பெருமையில் பீடுஉடையது இல்")

;  ["1021","1","பொருட்பால்","குடியியல்","குடிசெயல்வகை" "கருமம்" "செயஒருவன்" "கைதூவேன்" "என்னும்"  "பெருமையில்" "பீடுஉடையது" "இல்"])
; "22,2,அறத்துப்பால்,பாயிரம் ,நீத்தார் பெருமை, துறந்தார் பெருமை துணைக்கூறின் வையத்து இறந்தாரை எண்ணிக்கொண் டற்று"))
(->>
;   ["1021","1","பொருட்பால்","குடியியல்","குடிசெயல்வகை" "கருமம் செயஒருவன் கைதூவேன் என்னும்  பெருமையில் பீடுடைய தில்"])
   (str/split
     "12,1014,4,பொருட்பால்,குடியியல்,நாணுடைமை, அணியன்றோ நாணுடைமை சான்றோர்க் கஃதின்றேல் பிணியன்றோ பீடு நடை"
     ;"291,1,அறத்துப்பால்,துறவறவியல்,வாய்மை, வாய்மை எனப்படுவ தியாதெனின் யாதொன்றந் தீமை யிலாத சொலல்"
    #",")
  (kural-rec)
  (padal)
  (:op)
  (seer-tri-numbering))
  ;(prime-number?))
  ;(pa/thalais-explain-str))
  ;(pa/padal)
  ;(:seers))
