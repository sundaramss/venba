(ns yappu-ilakkanam.appcss
  (:require [cljss.reagent :as cr :refer-macros [defstyled]]
            [cljss.core :refer [inject-global defstyles]]))
(defn global-css []
 (inject-global
  {:container {
                :width "100%"
                :max-width "1400px"
                :margin  " 0 auto"
                :font-family "Helvetica"
                :font-size "14px"}}))

(defstyles H-MENUS []
 {:min-height "35px"
  :width "100%"
  :background "#ad6e47"})

(defstyles H-M-ROW []
 { :padding-left "16px"
   :text-align "right"});

(defstyles H-MENU []
 { :display "inline-block"
   :padding "8px 4px 0"})

(defstyles H-M-LINK []
   {:color "#fff"
    :text-decoration "none"
    :font-size "12px"})

(defstyled H1 :h1
  {:font-size "38px" ;; reads the value and removes custom `:size` attribute
   :color (with-meta #(get {:light "#fff" :dark "#000"} %) :color) ;; gets `:color` value and removes this attribute
   :padding (with-meta #(str %1 " " %2) [:padding-v :padding-h])}) ;; gets values of specified attrs as arguments and remove those attrs

(defstyles CONTENT []
  { :margin "0 auto"
    :background "#efefef"});

(defstyles FILTER-CONTAINER []
 {
   :max-width "1440px";
   :margin "0 auto"});

(defstyles K-F-TITLE []
 {
   :font "bold 24px monospace"
   :padding-left "10px"})

(defstyles K-F-WRAPPER []
 {
   :margin "20px 0"
   :display "inline-block"
   :width "100%"})

(defstyles K-F-DROPDOWN []
 {
  :display "inline-block"
  :width "20%"
  :float "left"
  :margin "10px"})

(defstyles K-F-SELECT [& {borderColor :border-color :or {borderColor "#a9a9a9"}}]
 {
  :width "100%"
  :height "36px"
  :border (when borderColor
            (str "1px solid " borderColor))})
