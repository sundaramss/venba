(ns yappu-ilakkanam.common)

(def avail #{"kural"})

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
 (if (empty? asaikal) ""
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
       (str mudhal mudivu))))
(defn paginate
  "Returns data requred to render paginator."
  [{:keys [records per-page max-pages current biased] :or {per-page 10 max-pages 10 current 1 biased :left}}]

  (let [total-pages (int (Math/ceil (/ records per-page)))
        half (Math/floor (/ max-pages 2))
        left-half  (int (if (= biased :left) (- half (if (odd? max-pages) 0 1)) half))
        right-half (int (if (= biased :right) (- half (if (odd? max-pages) 0 1)) half))
        virtual-start (- current left-half);can be a minus
        virtual-end (+ current right-half); can be exceeding than available page limit
        start (max 1 (- virtual-start (if (> virtual-end total-pages) (- virtual-end total-pages) 0)))
        end (inc (min total-pages (+ current (+ right-half (if (< virtual-start 1) (Math/abs (dec virtual-start)) 0)))))
        pages (range start end)
        previous (if (= (first pages) 1) false true)
        next (if (= (last pages) total-pages) false true)]
    {:current current :pages pages :previous previous :next next :total-pages total-pages}))

(defn padding [arr ct val]
 (let [l (if (empty? arr) [val] arr)]
    (first (partition ct ct (repeat val) l))))

(defn find-cols-size [sp]
 (let [maxColsRow (map count (apply max-key count sp))]
  (reduce (fn [ac it]
           (let [acCount (count ac)
                 numIt (map count it)
                 newIt (padding numIt acCount 0)]
            (map #(if (> %1 %2) %1 %2) ac newIt))) maxColsRow sp)))
