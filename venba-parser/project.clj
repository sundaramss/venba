(defproject venba-parser "0.1.0-SNAPSHOT"
  :description "Venba parser : parse the csv file and prepare venba"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/core.async "0.4.490"]]
;                 [metosin/spec-tools "0.9.2"]]

  :main ^:skip-aot venba-parser.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
