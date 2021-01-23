(defproject yappu-ilakkanam "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.741"
                  :exclusions [com.google.javascript/closure-compiler-unshaded
                               org.clojure/google-closure-library]]
                 [thheller/shadow-cljs "2.11.14"]
                 [reagent "0.10.0" :exclusions [cljsjs/react cljsjs/react-dom]]
                 [re-frame "1.1.2"]
                 [day8.re-frame/http-fx "0.2.2"]
                 [day8.re-frame/async-flow-fx "0.1.0"]
                 [bidi "2.1.6"]
                 [kibu/pushy "0.3.8"]]

  :plugins []

  :min-lein-version "2.5.3"

  :source-paths ["src/clj" "src/cljs"]

  :test-paths   ["test/cljs"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"
                                    "test/js"]


  :aliases {"dev"  ["with-profile" "dev" "run" "-m" "shadow.cljs.devtools.cli" "watch" "app"]
            "dev-repl"  ["with-profile" "dev" "run" "-m" "shadow.cljs.devtools.cli" "cljs-repl" "app"]
            "prod" ["with-profile" "prod" "run" "-m" "shadow.cljs.devtools.cli" "release" "app"]}

  :profiles
  {:dev
   {:dependencies [[binaryage/devtools "1.0.2"]
                   [day8.re-frame/re-frame-10x "0.7.0"]
                   [day8.re-frame/tracing "0.6.0"]]}

   :prod { :dependencies [[day8.re-frame/tracing-stubs "0.6.0"]]}})
