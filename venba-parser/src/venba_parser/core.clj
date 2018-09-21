(ns venba-parser.core
  (:require [venba-parser.kural :as kural])
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  ;(prn (kural/t)))
  ;(kural/kural-extra-data-setup "../data/kural.csv")
  (kural/kural-data-setup "../data/kural.csv"))
