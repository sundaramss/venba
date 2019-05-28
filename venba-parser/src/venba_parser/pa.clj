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
     (println val)
     (assoc {} :sp sp :ap ap :kn kn :w word))))



(ta-sol-asai "அம்மா")

(ta-sol-asai "அஃது")

(ta-sol-asai "அஃது" true)
