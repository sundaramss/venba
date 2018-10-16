(ns venba-parser.kural
  (:require [venba-parser.common :as common]
            [venba-parser.yappu :as yappu]
            [clojure.string :as str]
            [clojure.set :as cset]
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
