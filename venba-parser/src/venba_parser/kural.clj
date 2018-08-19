(ns venba-parser.kural
  (:require [venba-parser.common :as common] [clojure.string :as str] [clojure.java.io :as io]))

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


(defn read-kural [input-file] ""
  (with-open [rdr (io/reader input-file)]
   (let [lines (line-seq rdr)]
    (dorun
     (map #(write-kural %)
          (pmap prepare-json-kural lines))))))
