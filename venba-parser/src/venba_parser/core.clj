(ns venba-parser.core
  (:require [venba-parser.kural :as kural])
  (:gen-class))

(def k1 "1,1,அறத்துப்பால்,பாயிரம் ,கடவுள் வாழ்த்து, அகர முதல எழுத்தெல்லாம் ஆதி  பகவன் முதற்றே உலகு")
(def k1021 "1021,1,பொருட்பால்,குடியியல்,குடிசெயல்வகை, கருமம் செயஒருவன் கைதூவேன் என்னும்  பெருமையில் பீடுஉடையது இல்")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (kural/kural-extra-data-setup "../data/kural.csv"))
  ;(kural/kural-data-setup "../data/kural.csv"))
 ;(kural/kural-search-data "../data/kural.csv"))
 ;(kural/kural-search-data-val "../data/kural.csv"))
 ;(let [kural (kural/parse-kural-record k1021)]
 ;  (prn str (kural/padal kural))))
  ;(prn (kural/cookup-search-data {} k)))
