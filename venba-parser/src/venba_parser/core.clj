(ns venba-parser.core
  (:require [venba-parser.kural :as kural]
            ;[clojure.spec.alpha :as s])
            [clojure.core.async :refer [<!!] :as async])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  ;(println (s/conform ::kural/kural-csv-rec
  ;(println (kural/kural-rec [1304 4 "காமத்துப்பால்" "கற்பியல் புலவி" "ஊடி" "யவரை" "உணராமை" "வாடிய" "வள்ளி" "முதலரிந்" "தற்று"])))
  ;(kural/kural-write-search-data-val "../data/kural.csv"))
  ; (kural/kural-write-search-7data-val "../data/kural.csv"))
  ;(time (kural/kurals-write "../data/kural.csv")))
  ;(time (kural/kurals-write-parallel "../data/kural.csv")))
  ;(kural/kurals-validate "../data/kural.csv")
  (kural/kurals-number-system "../data/kural.csv"))
  ;(kural/kurals-validate-parallel "../data/kural.csv"))
  ;(kural/prepare-json-kural-extra k42))
  ;(kural/kural-extra-data-setup "../data/kural.csv"))
  ;(kural/kural-data-setup "../data/kural.csv"))
 ;(kural/kural-search-data "../data/kural.csv"))
 ;(kural/kural-search-data-val "../data/kural.csv"))
 ;(let [kural (kural/parse-kural-record k1021)]
 ;  (prn str (kural/padal kural))))
  ;(prn (kural/cookup-search-data {} k)))



;[1304 4 "காமத்துப்பால்" "கற்பியல் புலவி" "ஊடி" "யவரை" "உணராமை" "வாடிய" "வள்ளி" "முதலரிந்" "தற்று"]
