(ns venba-parser.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defn- ta-char [x]
   (char x))

(defn- ta-char-str [x]
   (str (char x)))

(defonce uyir (map ta-char
               [0x0B85 0x0B86 ; அ ஆ
                0x0B87 0x0B88 ;இ ஈ
                0x0B89 0x0B8A ;உ ஊ
                0x0B8E 0x0B8F ; எ ஏ
                0x0B90 ; ஐ
                0x0B92 0x0B93 ; ஒ ஓ
                0x0B94])) ; ஔ

(defonce ayitha [(char 0x0B83)])

(defonce ayitha-str [(str (char 0x0B83))])

(defonce thunai (map ta-char
                  [0x0BBE 0x0BBF 0x0BC0 ; ா ி ீ
                    0x0BC1 0x0BC2 0x0BC6 ; ு ூ ெ
                    0x0BC7 0x0BC8 0x0BCA ;  ே ை ொ
                    0x0BCB 0x0BCC 0x0BCD])) ; ோ  ௌ ்

(defonce ameyi-char (map ta-char
                      [0x0B95 0x0B99 ; க ங
                       0x0B9A 0x0B9C 0x0B9E ; ச ஜ ஞ
                       0x0B9F 0x0BA3 ;ட ண
                       0x0BA4 0x0BA8 ; த ந
                       0x0BA9 ; ன
                       0x0BAA 0x0BAE ; ப ம
                       0x0BAF 0x0BB0 0x0BB1 ; ய ர ற
                       0x0BB2 0x0BB3 0x0BB4 0x0BB5 ; ல ள ழ வ
                       0x0BB7 0x0BB8 0x0BB9])) ; ஷ ஸ ஹ

(defonce uyir-kuril (map  ta-char-str
                         [0x0B85 ;அ
                          0x0B87 ;இ
                          0x0B89 ;உ
                          0x0B8E ;எ
                          0x0B92]));ஒ

(defonce uyir-nedil (map  ta-char-str
                       [0x0B86 ;ஆ
                        0x0B88 ;ஈ
                        0x0B8A ;ஊ
                        0x0B8F ;ஏ
                        0x0B90 ;ஐ
                        0x0B93 ;ஓ
                        0x0B94])) ;ஔ

(def uyirmei-kuril (for [f ameyi-char s (map ta-char-str
                                           [0x0BBF ;  ி
                                            0x0BC1 ; ு
                                            0x0BC6 ;  ெ
                                            0x0BCA])] ;ொ
                      (str f s)))
;
(def uyirmei-nedil (for [f ameyi-char s (map ta-char-str
                                            [ 0x0BBE 0x0BC0 ; ா ீ
                                              0x0BC2; ூ
                                              0x0BC7 ;  ே
                                              0x0BCB])] ; ோ
                      (str f s)))

(def ameyi-str (map #(str %) ameyi-char))

(s/def ::meyi (set (for [f ameyi-char s [(char 0x0BCD)]]
                     (str f s))))

;(s/def ::meyi (set  (for [f ameyi-char s [(char 0x0BCD)]]
;                      (str f s)))

(s/def ::ta-first (set (reduce #(into %1 %2) [] [uyir ameyi-char ayitha])))

(s/def ::ta-thunai (s/? (set thunai)))

(s/def ::ta-split (s/* (s/cat :f ::ta-first :s ::ta-thunai)))

(s/def ::kuril-osai (set (reduce #(into %1 %2) [] [uyir-kuril ameyi-str uyirmei-kuril])))

(s/def ::nedil-osai (set (reduce #(into %1 %2) [] [uyir-nedil uyirmei-nedil])))

(s/def ::i-osai (set (for [f ameyi-char s [(char 0x0BC8)]] ;ை
                      (str f s))))

(s/def ::avu-osai (set (for [f ameyi-char s [(char 0x0BCC)]]; ௌ
                        (str f s))))

(s/def ::agg-osai (set ayitha-str))

(s/def ::ta-K-N (s/or
                        :N #(or (s/valid? ::nedil-osai %)
                                (s/valid? ::i-osai %)
                                (s/valid? ::avu-osai %))
                        :K ::kuril-osai))

(s/def ::ta-*-K-N-O (s/or
                        :K #(or (s/valid?  ::kuril-osai %)
                                (s/valid? ::i-osai %)
                                (s/valid? ::avu-osai %))
                        :N ::nedil-osai
                        :O #(or (s/valid? ::meyi %)
                                (s/valid? ::agg-osai %))))

(s/def ::ta-K-N-O (s/cat :f ::ta-K-N :r (s/* ::ta-*-K-N-O)))

(s/def ::ta-_-K-N-O (s/or
                        :K #(or (s/valid?  ::kuril-osai %)
                                (s/valid? ::i-osai %)
                                (s/valid? ::avu-osai %)
                                (s/valid? ::agg-osai %))
                        :N ::nedil-osai
                        :O ::meyi))

(s/def ::ta-K-N-O-agg (s/cat :f ::ta-K-N :r (s/* ::ta-_-K-N-O)))

(defn ta-split [word]
 (let [arrMap (s/conform ::ta-split (seq word))]
  (if (= arrMap ::s/invalid) []
   (map (fn [{f :f s :s}] (str f s)) arrMap))))

(defn ta-kuril-nedil [word]
  (let [ta (ta-split word)
        kn (s/conform ::ta-K-N-O ta)
        kn-1 (conj [] (:f kn))
        kres (into kn-1 (:r kn))]
   kres))

(defn ta-kuril-agg-nedil [word]
  (let [ta (ta-split word)
        kn (s/conform ::ta-K-N-O-agg ta)
        kn-1 (conj [] (:f kn))
        kres (into kn-1 (:r kn))]
   kres))

(defn- kuril? [[v _]]
  (= v :K))

(defn- nedil? [[v _]]
  (= v :N))

(defn- ottru? [[v _]]
  (= v :O))

(s/def ::asai (s/* (s/alt :NE
                         (s/alt
                          :f (s/& (s/cat :1 nedil? :2 ottru? :3 ottru?))
                          :e (s/& (s/cat :1 kuril? :2 ottru? :3 ottru?))
                          :d (s/& (s/cat :1 nedil? :2 ottru?))
                          :c (s/& (s/cat :1 kuril? :2 ottru?))
                          :a (s/& (s/cat :1 nedil?))
                          :b (s/& (s/cat :1 kuril?)))
                       :NI
                        (s/alt
                          :f (s/& (s/cat :1 kuril? :2 nedil? :3 ottru? :4 ottru?))
                          :e (s/& (s/cat :1 kuril? :2 kuril? :3 ottru? :5 ottru?))
                          :d (s/& (s/cat :1 kuril? :2 nedil? :3 ottru?))
                          :c (s/& (s/cat :1 kuril? :2 kuril? :3 ottru?))
                          :a (s/& (s/cat :1 kuril? :2 nedil?))
                          :b (s/& (s/cat :1 kuril? :2 kuril?))))))

(defn- cleanup [[_ v]]
    (vals v))

(defn ta-asai
  ([w] (ta-asai w false))
  ([w b] (let [fun (if b ta-kuril-agg-nedil  ta-kuril-nedil)
                value (s/conform ::asai (fun w))]
           (if (= ::s/invalid value) []
            (map (fn [[f val]] (into (conj [] f) (cleanup val))) value)))))

(s/def ::kural-parse (s/* (s/cat :ve (s/+ number?)
                                 :ave (s/+ number?))))

(s/def ::ma-seer #{[:NE :NE] [:NI :NE]})
(s/def ::vilam-seer #{[:NE :NI] [:NI :NI]})
(s/def ::kai-seer #{[:NE :NE :NE] [:NI :NE :NE]
                    [:NE :NI :NE] [:NI :NI :NE]})

(s/def ::etru-seer #{[:NE :NE] [:NI :NE] [:NI] [:NE]})

(s/def ::kural-seers (s/cat :seerkal (s/* (s/or :ma ::ma-seer :vilam ::vilam-seer :kai ::kai-seer))
                         :etru ::etru-seer))

(defn- isFirstNeerai? [[_ [v _]]]
  (= v :NI))

(defn- isFirstNeer?  [[_ [v _]]]
  (= v :NE))

(defn- isMa? [[v _]]
 (= v :ma))

(defn- isVilam? [[v _]]
  (= v :vilam))

(defn- isKai? [[v _]]
  (= v :kai))

(s/def ::ma-thalai? (s/cat :f isMa? :s isFirstNeerai?))
(s/def ::vilam-thalai? (s/cat :f isVilam? :s isFirstNeer?))
(s/def ::kai-thalai? (s/cat :f isKai? :s isFirstNeer?))
(s/def ::ven-thalai (s/coll-of (s/or :m ::ma-thalai? :v ::vilam-thalai? :k ::kai-thalai?)))

(defn venba-seers [parr]
  (let [k (s/conform ::kural-seers parr)
        k? (= ::s/invalid k)]
   (if k? nil k)))

(defn venba-thalais [kuralSeers]
  (if (nil? kuralSeers) nil
    (let [seerkal (:seerkal kuralSeers)
          etru (:etru kuralSeers)
          fv (partition 2 1 (into seerkal [[:last etru]]))
          vthalai (s/conform ::ven-thalai fv)]
      (if (= ::s/invalid vthalai) nil
          vthalai))))

(defn venba-thalais-error [kuralSeers]
  (if (nil? kuralSeers) nil
    (let [seerkal (:seerkal kuralSeers)
          etru (:etru kuralSeers)
          fv (partition 2 1 (into seerkal [[:last etru]]))
          vthalai (s/explain-data ::ven-thalai fv)
          index (get-in (first (::s/problems vthalai)) [:in 0])]
      (println (nth (get-in vthalai [::s/value]) index))
      vthalai)))

(defn isAytha? [c]
  (= (first ayitha) c))

;(venba-seers [[:NI :NE] [:NE :NE] [:NI :NE :NE] [:NE :NE] [:NI :NE] [:NI :NE] [:NI :NE]])
;(venba-thalais (venba-seers [[:NI :NE] [:NE :NE] [:NI :NE :NE] [:NE :NE] [:NI :NE] [:NI :NE] [:NI :NE]]))
;(ta-split "அம்மா")
;(ta-split "மெய்ப்பொருள்")
;(ta-split "அஃது")
;(ta-split "Hello")
;(ta-kuril-nedil "அம்மா")
;(ta-kuril-nedil "மெய்ப்பொருள்")
;(ta-kuril-nedil "தங்கை")
;(ta-kuril-nedil "கைதி")
;(ta-kuril-nedil "பகை")
;(ta-kuril-nedil "அஃது")
;(ta-kuril-agg-nedil "அஃது")
;(s/conform ::ta-*-K-N-O (first (ta-split "அஃது")))
;(s/explain-str ::ta-K-N-O (ta-split "அஃது"))
;(s/explain-str (s/or
;                        :N #(or (s/valid? ::nedil-osai %)
;                                (s/valid? ::i-osai %)
;                                (s/valid? ::avu-osai %)
;                        :K ::kuril-osai) (first (ta-split "அஃது")))
;(s/explain-str (s/* (s/cat :f ::ta-first))  ["அ" "ஃ"])
;(s/conform ::kural-seers
; [[:NI :NE][:NI :NI :NE][:NE :NE :NE][:NE :NE] [:NI :NI][:NE :NI :NE] [:NE]])

;(s/def ::ma-thalai? (s/cat :f isMa? :s isFirstNeerai?))
;(s/def ::vilam-thalai? (s/cat :f isVilam? :s isFirstNeer?))
;(s/def ::kai-thalai? (s/cat :f isKai? :s isFirstNeer?))
;(s/def ::ven-thalai (s/coll-of (s/or :m ::ma-thalai? :v ::vilam-thalai? :k ::kai-thalai?)))

;(s/conform ::ma-thalai? [[:ma [:NE :NE]] [:vilam [:NI :NI]]])
;(s/conform ::vilam-thalai? [[:vilam [:NI :NE]] [:ma [:NE :NE]]])
;(s/conform ::kai-thalai? [[:kai [:NI :NE :NE]] [:ma [:NE :NE]]])

;(s/conform ::ven-thalai [(list [:ma [:NI :NE]] [:kai [:NI :NI :NE]]) (list [:ma [:NI :NE]] [:kai [:NI :NI :NE]])])

;(def inp [[:NI :NE][:NI :NI :NE][:NE :NE :NE][:NE :NE] [:NI :NI][:NE :NI :NE] [:NE]])

;(let [v (s/conform ::kural-seers inp)
;      s (:seerkal v)
;      e (:etru v)
;      fv (into s [[:last e]]) fv))

;(:seerkal (s/conform ::kural-seers inp))
;(s/valid? ::ven-thalai (partition 2 1 (:seerkal (s/conform ::kural-seers inp))))
;(s/explain-str ::ta-split (seq "எழுத்தெல்லாம்"))
;(s/conform ::ta-K-N-O (ta-split "எழுத்தெல்லாம்"))
;(ta-kuril-nedil "எழுத்தெல்லாம்")
;(ta-asai "எழுத்தெல்லாம்")
;(ta-kuril-nedil "அம்மா")
;(ta-asai "அம்மா")
;(ta-asai "அஃது")
;(ta-asai "அஃது" true)
