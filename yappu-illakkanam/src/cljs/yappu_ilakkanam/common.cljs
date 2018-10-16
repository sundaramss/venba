(ns yappu-ilakkanam.common)

(defn getOpt [{search :s, :or {search []}} id]
  (some #(if (= (% :id) id) (% :opt)) search))

(defn getOptIdx [{search :s, :or {search []}} id]
  (first (keep-indexed #(if (= (%2 :id) id) %1) search)))

(def asai {:NE "நே" :NI "நி"})

(defn etru-seer [asaikal]
   (let [asaiJoin #(apply str (map name %))
         mudhal (condp contains? (asaiJoin (take 2 asaikal))
                 #{"NE"} "நாள்"
                 #{"NI"} "மலர்"
                 #{"NENE"} "காசு"
                 #{"NINE"} "பிறப்பு")] mudhal))

(def seerkal {1 [:NE] 2 [:NI] 3 [:NE :NE] 4 [:NI :NE]
              5 [:NE :NE] 6 [:NI :NE] 7 [:NE :NI] 8 [:NI :NI] 
              9  [:NE :NE :NE] 10 [:NI :NE :NE] 11 [:NE :NI :NE] 12 [:NI :NI :NE]
              13 [:NE :NE :NI] 14 [:NI :NE :NI] 15 [:NI :NI :NI] 16 [:NE :NI :NI]
              17 [:NE :NE :NE :NE] 18 [:NE :NE :NE :NI] 19 [:NE :NE :NI :NE]  20 [:NE :NE :NI :NI]
              21 [:NI :NE :NE :NE] 22 [:NI :NE :NE :NI] 23 [:NI :NE :NI :NE]  24 [:NI :NE :NI :NI]
              25 [:NE :NI :NE :NE] 26 [:NE :NI :NE :NI] 27 [:NE :NI :NI :NE]  28 [:NE :NI :NI :NI]
              29 [:NI :NI :NE :NE] 30 [:NI :NI :NE :NI] 31 [:NI :NI :NI :NE]  32 [:NI :NI :NI :NI]})

(defn asai-seer [asaikal]
   (let [asaiJoin #(apply str (map name %))
         mudhal (condp contains? (asaiJoin (take 2 asaikal))
                 #{"NENE"} "தேமா"
                 #{"NINE"} "புளிமா"
                 #{"NINI"} "கருவிள"
                 #{"NENI"} "கூவிள")
         mudivuAsai  (last asaikal)
         mudivu (condp = (count asaikal)
                 1 ""
                 2 :>> (fn [_] (when (= :NI mudivuAsai) "ம்"))
                 3 :>> (fn [_] (condp = mudivuAsai
                                     :NE "ங்காய்"
                                     :NI "ங்கனி"))
                 4 :>> (fn [_] (condp contains? (asaiJoin (take-last 2 asaikal))
                                 #{"NENE"} "ந்தண்பூ"
                                 #{"NINE"} "நறும்பூ"
                                 #{"NINI"} "நறுநிழல்"
                                 #{"NENI"} "ந்தண்ணிழல்")))]
       (str mudhal mudivu)))
