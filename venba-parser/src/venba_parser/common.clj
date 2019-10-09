(ns venba-parser.common
  (:require [clojure.java.io :as io] [clojure.data.json :as json]))

(defn make-dir [parents]
 "This will receive array and create nested directory except last item"
 (io/make-parents parents))

(defn clj-to-json [data] ""
 (json/write-str data))

(defn write-venba [path padal]
 (prn path)
 (with-open [w (clojure.java.io/writer  path)]
  (.write w padal)))
