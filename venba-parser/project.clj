(defproject venba-parser "0.1.0-SNAPSHOT"
  :description "Venba parser : parse the csv file and prepare venba"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"][org.clojure/data.json "0.2.6"]]
  :main ^:skip-aot venba-parser.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
