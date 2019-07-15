(ns venba-parser.pa
  (:require [venba-parser.common :as common]
            [venba-parser.spec :as vpspec]
            [clojure.string :as str]
            [clojure.set :as cset]
            [clojure.java.io :as io]))



;val  ([:K "அ"] [:O "ம்"])
(defn- asai-join [val]
    (reduce #(str %1 (last %2)) nil val))
;(sp-join [[:K "அ"] [:O "ம்"]])

(defn- osai-join [val]
    (reduce #(conj %1 (first %2)) [] val))


(defn sp-join [val]
    (vec (map #(asai-join (rest %)) val)))

;val  ([:NE [:K "அ"] [:O "ம்"]] [:NE [:N "மா"]])
;(sp-join [[:NE [:K "அ"] [:O "ம்"]] [:NE [:N "மா"]]])

(defn ap-join [val]
  (vec (map #(first %) val)))

(defn op-join [val]
  (vec (map #(osai-join (rest %)) val)))


;(ap-join [[:NE [:K "அ"] [:O "ம்"]] [:NE [:N "மா"]]])

(defn ta-sol-asai
  ([word] (ta-sol-asai word false))
  ([word b]
   (let [val (vpspec/ta-asai word b)
         sp  (sp-join val)
         ap  (ap-join val)
         kn  (op-join val)]
     (assoc {} :sp sp :ap ap :kn kn :w word))))

(vpspec/ta-asai "எழுத்தெல்லாம்")

(ta-sol-asai "அம்மா")

(ta-sol-asai "அஃது")

(ta-sol-asai "அஃது" true)

(defn get-kv [adi k]
  (println (k adi))
  (vec (map #(k %) adi)))

(defn padal
 ([p] (padal p false))
 ([{padal :padal iyal  :iyal adhikaram :adhikaram pal :pal ve :ve} lookAytha]
  (let [adikal (map #(ta-sol-asai % lookAytha) (str/split (str/trim (first padal)) #" +"))
        records {:sp [] :ap [] :op [] :iyal iyal :adhikaram adhikaram :pal pal :ve ve :seers nil :thalai nil}
        sprecs (update-in records [:sp] into (map #(:sp %) adikal))
        aprecs (update-in sprecs [:ap] into (map #(:ap %) adikal))
        oprecs (update-in aprecs [:op] into (map #(:kn %) adikal))
        seerrecs (assoc oprecs :seers (vpspec/venba-seers (:ap oprecs)))
        thalairecs (assoc seerrecs :thalai (vpspec/venba-thalais (:seers seerrecs)))]
     thalairecs)))
       ; seerrecs (update-in oprecs [:seers] (vpspec/venba-seers (:ap oprecs)))
       ; thalairecs (update-in oprecs [:thalai] (vpspec/venba-thalais (:seers seerrecs)))]
    ;thalairecs))

(defn thalais-explain-str [result]
     (vpspec/venba-thalais-error (:seers result)))

(defn isAytha? [value]
  (some vpspec/isAytha? (seq value)))
