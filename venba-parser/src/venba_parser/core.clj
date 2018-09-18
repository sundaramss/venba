(ns venba-parser.core
  (:require [venba-parser.kural :as kural])
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  ;(prn (kural/t)))
  ;(kural/prepare-json-kural-extra "இருள்சேர் இருவினையும் சேரா இறைவன் பொருள்சேர் புகழ்புரிந்தார் மாட்டு")
  ;(kural/prepare-json-kural-extra "24,4,அறத்துப்பால்,பாயிரம் ,நீத்தார் பெருமை,உரனென்னும் தோட்டியான் ஓரைந்தும் காப்பான்  வரனென்னும் வைப்பிற்கோர் வித்து"))
  ;(kural/prepare-json-kural-extra "330,10,அறத்துப்பால்,துறவறவியல்,கொல்லாமை,உயிர் உடம்பின் நீக்கியார் என்ப   செயிர் உடம்பின் செல்லாத்தீ வாழ்க்கை யவர்"))
  (kural/kural-extra-data-setup "/Users/sundaramss/project/venba/data/kural/kural.csv"))
