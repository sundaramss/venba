(ns yappu-ilakkanam.routes
  (:require-macros [secretary.core :refer [defroute]])
  (:import goog.History)
  (:require
   [secretary.core :as secretary]
   [goog.events :as gevents]
   [goog.history.EventType :as EventType]
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.subs :as subs]))


(defn hook-browser-navigation! []
  (doto (History.)
    (gevents/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defn set-hash! [loc]
  (js/console.log "Set-Hash" loc)
  (set! (.-hash js/window.location) loc))

(defn app-routes []
  (secretary/set-config! :prefix "#")
  ;; --------------------
  ;; define routes3yy here
  (defroute "/" []
    (js/console.log "Default root /")
    (set-hash! "/kural/1"))

  ;; define routes here
  (defroute "/kural/:aidx" [aidx]
   (let [idx (js/parseInt (or (not-empty (clojure.string/join (re-seq #"\d+" aidx))) "1"))
         sidx (cond (>= 0 idx) 1 :else (mod idx 134))]
    (if (= sidx idx)
     (re-frame/dispatch [::events/load-range-kural sidx])
     (set-hash! (str "/kural/" sidx)))))

  ;; define routes here
  (defroute "/kural/s/*p" [p]
    (re-frame/dispatch [::events/search-kural p]))

  (defroute "/about" []
    (re-frame/dispatch [::events/set-active-panel :about-panel]))

  ;; --------------------
  (hook-browser-navigation!))
