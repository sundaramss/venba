(ns venba-parser.yappu
  (:require [venba-parser.common :as common] [clojure.string :as str] [clojure.java.io :as io]))

(def thunai (set (map char (range 0x0bbe 0x0bce))))

(def kuril (set (map char [0x0B85 0x0B87 0x0B89 0x0B8e 0x0B90 0x0B92 0x0B94 0x0BBF 0x0BC1 0x0BC6 0x0B95 0x0B99 0x0B9A 0x0B9E 0x0B9F 0x0BA3 0x0BA4 0x0BA8 0x0BAA 0x0BAE 0x0BAF 0x0BB0 0x0BB2
                                 0x0BB5 0x0BB4 0x0BB3 0x0BB1 0x0BCA 0x0BA9 0x0BC8 0x0BCC])))
(def nedil (set (map char [0x0B86 0x0B88 0x0B8A 0x0B8F  0x0B93
                             0x0BBE 0x0BC0 0x0BC2 0x0BC7  0x0BCB])))

(def ottrue (set (map char [0x0BCD 0x0B83])))

(def mudhalNedil (set (concat nedil (map char [0x0B90 0x0B94 0x0BC8 0x0BCC]))))

(def asaikal #{"K" "KK" "N" "KN"})


(defn tamil-ezhuthu-split
     ([sol] (let [ezhuthukal (map #(.charAt % 0) (str/split sol #""))
                  isThunai (contains? thunai (first ezhuthukal))]
             (tamil-ezhuthu-split [] nil (first ezhuthukal) (rest ezhuthukal) isThunai)))
     ([result temp current rem isThunai] (if (empty? rem)
                                          (if isThunai
                                              (conj result (str temp current))
                                              (common/if-conj (common/if-conj result temp) current))
                                          (let [tmp (if-not isThunai current)
                                                nextCurrent (first rem)
                                                isNextThunai (contains? thunai nextCurrent)
                                                res (if isThunai (conj result (str temp current))
                                                                 (common/if-conj result temp))]
                                           (recur res tmp nextCurrent (rest rem) isNextThunai)))))

(defn mathirai [ezhuthu isMuthal]
    (let [lNedil (if isMuthal mudhalNedil nedil)
          atcharam (map #(.charAt % 0) (str/split ezhuthu #""))
          atcharamcount (count atcharam)
          fatcharam (first atcharam)
          satcharam (second atcharam)
          findOsai #(cond
                     (contains? %1 %2) :N
                     (contains? kuril %2) :K
                     (contains? ottrue %2) :O)]
     ;(prn (str ezhuthu "****" atcharamcount ">>>" fatcharam satcharam))
     (condp = atcharamcount
       1 (findOsai lNedil fatcharam)
       2 (findOsai lNedil satcharam))))

(defn asai-peeri [osaikal]
 (let [seen (atom true) tmp (atom "")]
   (partition-by #(let [v (name %)]
                     (if (= :O %)
                      (do (reset! tmp (str @tmp v)) @seen)
                      (do (if-not (contains? asaikal (str @tmp v))
                                  (do (reset! tmp "") (reset! seen (not @seen)))
                                  (do (reset! tmp (str @tmp v)) @seen)))))
                 osaikal)))

(defn seer-nilai [asai]
  (condp contains? (apply str (map name (take 2 asai)))
   #{"K" "KO" "N" "NO"} :NE
   #{"KK" "KN"} :NI))

(defn osaikal-piri [ezhuthukal] (map-indexed #(mathirai %2 (= %1 0)) ezhuthukal))

(defn asaikal-name [asaikal]
    (map seer-nilai asaikal))

(defn asai-seer [asaikal]
   (prn asaikal)
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
                 4 :>> (fn [_] (prn (take 2 asaikal))
                               (condp contains? (asaiJoin (take-last 2 asaikal))
                                 #{"NENE"} "ந்தண்பூ"
                                 #{"NINE"} "நறும்பூ"
                                 #{"NINI"} "நறுநிழல்"
                                 #{"NENI"} "ந்தண்ணிழல்")))]
       (str mudhal mudivu)))

(defn etru-seer [asaikal]
   (let [asaiJoin #(apply str (map name %))
         mudhal (condp contains? (asaiJoin (take 2 asaikal))
                 #{"NE"} "நாள்"
                 #{"NI"} "மலர்"
                 #{"NENE"} "காசு"
                 #{"NINE"} "பிறப்பு")] mudhal))

(defn sol-asai-piri
   ([sol] (let [tes (tamil-ezhuthu-split sol)
                 ap (-> tes
                        osaikal-piri
                        asai-peeri)
                 apc (map count ap)]
              (prn ap)
              (sol-asai-piri [] tes apc)))
   ([result tes apc] (let [ d (split-at (first apc) tes)
                            r (conj result (apply str (first d)))]
                       (if (= (count apc) 1)
                           r
                           (recur r (last d) (rest apc))))))

(defn asaiPerkal [x] (-> x
                      (tamil-ezhuthu-split)
                      (osaikal-piri)
                      (asai-peeri)
                      (asaikal-name)))

(defn invert-many-to-one
  "returns a one-to-many mapping"
  ([m] (invert-many-to-one #{} m))
  ([to m]
   (persistent!
    (reduce (fn [m [k v]]
              (assoc! m v (conj (get m v to) k)))
            (transient {}) m))))
