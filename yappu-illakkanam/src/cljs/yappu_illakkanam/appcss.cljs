(ns yappu-illakkanam.appcss
  (:require [cljss.reagent :as cr :refer-macros [defstyled]]))


(defstyled h1 :h1
  {:font-family "sans-serif"
   :font-size :size ;; reads the value and removes custom `:size` attribute
   :color (with-meta #(get {:light "#fff" :dark "#000"} %) :color) ;; gets `:color` value and removes this attribute
   :padding (with-meta #(str %1 " " %2) [:padding-v :padding-h])}) ;; gets values of specified attrs as arguments and remove those attrs

(defn hello [] "Hello World")
