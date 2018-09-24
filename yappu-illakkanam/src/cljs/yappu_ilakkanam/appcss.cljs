(ns yappu-ilakkanam.appcss
  (:require [cljss.reagent :as cr :refer-macros [defstyled]]
            [cljss.core :refer [inject-global]]))
(defn global-css []
 (inject-global
  {:container {
                     :width "100%"
                     :max-width "1800px"
                     :margin  " 0 auto"
                     :font-family "Helvetica"
                     :font-size "14px"}}))
(defstyled LOGO :img
 {:width "200px"
  :height "auto"})

(defstyled H-MENUS :div
 {:min-height "35px"
  :width "100%"
  :background "#ad6e47"})

(defstyled H-M-ROW :ul
 { :padding-left "16px"
   :text-align "right"});

(defstyled H-MENU :li
 { :display "inline-block"
   :padding "8px 4px 0"})

(defstyled H-M-LINK :a
   {:color "#fff"
    :text-decoration "none"
    :font-size "12px"})

(defstyled H1 :h1
  {:font-size "38px" ;; reads the value and removes custom `:size` attribute
   :color (with-meta #(get {:light "#fff" :dark "#000"} %) :color) ;; gets `:color` value and removes this attribute
   :padding (with-meta #(str %1 " " %2) [:padding-v :padding-h])}) ;; gets values of specified attrs as arguments and remove those attrs
