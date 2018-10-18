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
   :float "left"
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

(defstyles CHIP []
 {:display "inline-block"
  :padding "5px"
  :padding-left "15px"
  :padding-right "7.5px"
  :background "#555"
  :color "#fff"
  :font-weight "bold"
  :border-radius "15px 0 0 15px"
  :position "relative"
  :margin "0 16px"})

(defstyles C-CHIP []
 {:float "left"
  :padding "8px 16px"})

(defstyles P-C-CHIP []
 {:width "100%" :float "left"})

(defstyles EN []
 {
  :padding "2px 5px"
   :background "#efefef"
   :border-radius "8px"
   :color "#555"
   :margin-right "7px"
   :display "inline-block"
   :text-align "center"
   :font-size "12px"})

(defstyles CROSS []
 {
   :outline "none"
   :background "#555"
   :color "#fff"
   :border "0"
   :border-radius "0 15px 15px 0"
   :padding "5px 10px"
   :cursor "pointer"
   :position "absolute"
   :top "0"
   :bottom "0"
   :right "-25px"
   :line-height "0.5"
   :font-weight "bold"})

(defstyles KURALS []
  { :background "#fff"})

(defstyles KURAL-LIST []
  {
    :margin "0 auto"})

(defstyles KURAL []
   {
    :padding "24px"
    :float "left"
    :border-bottom "1px solid #f9f8f5"
    :width "100%"
    "&:nth-child(odd)" {:background "#f9f8f5"}
    "&:nth-child(even)" {:background "#e6e6e6"}})

(defstyles KURAL-PATH-C []
  {:padding "16px 0 0"})

(defstyles KURAL-PATH []
  { :min-width "150px"
    :padding-bottom "6px"})

(defstyles K-P-ITEM []
  { :font-size "12px"
    :color "#ad6e47"})

(defstyles K-ADI []
  { ;:float "left"
    :display "table"})

(defstyles K-ADI-1 []
  { ;:float "left"
    :display "table"})

(defstyles K-ADI-2 []
  { ;:float "left"
    :display "table"
    :padding-bottom "7px"})

(defstyles K-A-SEER [f]
 { :padding-right "36px"
   :display "table-cell"
   :border "1px solid #a0a0a0"
   :padding "16px"
   :width "132px"
   :background "#fff"
   :font-weight "bold"
   :border-left (if-not f "0"
                          "1px solid #a0a0a0")})

(defstyles K-A-S-1 [f]
 { :padding-right "36px"
   :display "table-cell"
   :padding "8px 16px"
   :width "132px"
   :background "#fff"
   :border-bottom "1px dashed #a0a0a0"
   :border-right "1px dashed #a0a0a0"
   :border-left (if-not f "0"
                          "1px dashed #a0a0a0")})
(defstyles UL-P-TOP []
 {
  ;:padding "0px"
  ;:margin "0px"
  :position "absolute"
  :display "inline-block"
  :padding-left "27px"
  :z-index 1})
  ;:bottom "-37px"
  ;:left "27px"})

(defstyles UL-P-BOTTOM []
 {
  ;:padding "0px"
  ;:margin "0px"
  ;:position "absolute"
  :display "inline-block"
  :padding-left "27px"})
  ;:bottom "-37px"
  ;:left "27px"})

(defstyles UL-P []
  {
    :display  "inline-block"
    :background "#fff"
    :border "1px solid #a0a0a0"})

(defstyles UL-LI-P []
  { :display  "inline-block"
    :border-right "1px solid #a0a0a0";
    "&:last-child" {:border-right "0px"}})

(defstyles UL-LI-A-P [active]
  { :color (if active "white" "black")
    :background-color (when active "#ad6e47")
    :display "block"
    :padding "8px 16px"
    :text-decoration "none"
    :&:hover {:background-color (if active "#ad6e47" "#DDD")}})
