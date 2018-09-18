(ns venba-parser.common
  (:require [clojure.java.io :as io] [clojure.data.json :as json]))

(defn make-dir [parents]
 "This will receive array and create nested directory except last item"
 (apply io/make-parents parents))

(defn clj-to-json [data] ""
 (json/write-str data))

(defn write-venba [path padal]
 (prn path)
 (with-open [w (clojure.java.io/writer  path)]
  (.write w padal)))

(defn if-conj
  "Conj item to coll iff item is not nil."
  [coll item]
  (if (not (nil? item))
    (conj coll (str item))
    coll))

(defn invert-many-to-one
  "returns a one-to-many mapping"
  ([m] (invert-many-to-one #{} m))
  ([to m]
   (persistent!
    (reduce (fn [m [k v]]
              (assoc! m v (conj (get m v to) k)))
            (transient {}) m))))
