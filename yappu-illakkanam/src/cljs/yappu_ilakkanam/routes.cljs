(ns yappu-ilakkanam.routes
  (:require-macros [secretary.core :refer [defroute]])
  (:import goog.History)
  (:require
   [secretary.core :as secretary]
   [goog.events :as gevents]
   [goog.history.EventType :as EventType]
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.common :as common]
   [yappu-ilakkanam.subs :as subs]))


(defn hook-browser-navigation! []
  (doto (History.)
    (gevents/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defn set-hash! [loc]
  (set! (.-hash js/window.location) loc))

(defn app-routes []
  (secretary/set-config! :prefix "#")
  ;; --------------------
  ;; define routes3yy here
  (defroute "/" []
    (js/console.log "Default root /")
    (set-hash! "/kural/1"))

  ;; define routes here
  (defroute "/:padal/:aidx" [padal aidx]
   (if (not (contains? common/avail padal)) (set-hash! "/kural/1")
    (let [idx (js/parseInt (or (not-empty (clojure.string/join (re-seq #"\d+" aidx))) "1"))
          sidx (cond (>= 0 idx) 1 :else (mod idx 134))]
     (if (= sidx idx)
      (re-frame/dispatch [::events/init-load-config sidx padal])
      (set-hash! (str "/" padal "/" sidx))))))

  ;; define routes here
  (defroute "/:padal/s/*p" [padal p query-params]
   (if (not (contains? common/avail padal)) (set-hash! "/kural/1")
    (let [page (or (:p query-params) 1)]
     (re-frame/dispatch [::events/search-kural p page padal]
      (set-hash! (str "/" padal "/s/" p "?p=" page))))))

  (defroute "/about" []
    (re-frame/dispatch [::events/set-active-panel :about-panel]))

  ;; --------------------
  (hook-browser-navigation!))
