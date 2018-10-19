(ns venba-parser.core
  (:require [venba-parser.kural :as kural])
  (:gen-class))

(def k1 "1,1,அறத்துப்பால்,பாயிரம் ,கடவுள் வாழ்த்து, அகர முதல எழுத்தெல்லாம் ஆதி  பகவன் முதற்றே உலகு")
(def k1021 "1021,1,பொருட்பால்,குடியியல்,குடிசெயல்வகை, கருமம் செயஒருவன் கைதூவேன் என்னும்  பெருமையில் பீடுஉடையது இல்")
(def k42 "42,2,அறத்துப்பால்,இல்லறவியல்,இல்வாழ்க்கை, துறந்தார்க்கும் தூவா தவர்க்கும் இறந்தார்க்கும் இல்வாழ்வான் என்பான் துணை")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  ;(kural/kural-write-search-data-val "../data/kural.csv")
  ;(kural/kural-write-search-7data-val "../data/kural.csv"))
  ;(kural/prepare-json-kural-extra k42))
  (kural/kural-extra-data-setup "../data/kural.csv"))
  ;(kural/kural-data-setup "../data/kural.csv"))
 ;(kural/kural-search-data "../data/kural.csv"))
 ;(kural/kural-search-data-val "../data/kural.csv"))
 ;(let [kural (kural/parse-kural-record k1021)]
 ;  (prn str (kural/padal kural))))
  ;(prn (kural/cookup-search-data {} k)))
