(ns venba-parser.core
  (:require [venba-parser.kural :as kural])
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (kural/read-kural "/Users/sundaramss/project/venba/data/kural.csv"))
