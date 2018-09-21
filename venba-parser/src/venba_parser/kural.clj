(ns venba-parser.kural
  (:require [venba-parser.common :as common]
            [venba-parser.yappu :as yappu]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn- parse-kural-record [kurlrec]
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

(defn kural-alakidu [seerkal]
   (let [ kuralasaikal (map yappu/asaiPerkal seerkal)
           etruseer (last kuralasaikal)
           seers (drop-last kuralasaikal)
           a (map yappu/asai-seer seers)
           b (yappu/etru-seer etruseer)]
     (conj (vec a) b)))

(defn kural-extra [{seerkal :padal ave :ave ve :ve}]
  (prn seerkal)
  (let [sp (map yappu/sol-asai-piri seerkal)
        op (map #(-> % (yappu/tamil-ezhuthu-split) (yappu/osaikal-piri) (yappu/asai-peeri)) seerkal)
        al (kural-alakidu seerkal)
        ap (map yappu/asaiPerkal seerkal)]
     (assoc {} :sp sp :op op :ap ap :al al :ave ave :ve ve)))

(defn padal [{padal :padal ave :ave ve :ve}]
    (prn ve ave)
    (assoc {} :ve ve :ave ave :padal (apply concat padal)))

(defn prepare-json-kural-extra [kuralrec]
 (->> kuralrec
  (parse-kural-record)
  (padal)
  (kural-extra)
  ((juxt (juxt kural-extra-json-file kural-extra-json)))))

(defn kural-extra-data-setup [input-file] ""
  (with-open [rdr (io/reader input-file)]
   (let [lines (line-seq rdr)]
    (dorun
     (map #(write-kural %)
          (map prepare-json-kural-extra lines))))))
