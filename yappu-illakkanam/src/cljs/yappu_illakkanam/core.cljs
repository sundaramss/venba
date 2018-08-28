(ns yappu-illakkanam.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [cljss.core :as css]
   [yappu-illakkanam.events :as events]
   [yappu-illakkanam.routes :as routes]
   [yappu-illakkanam.views :as views]
   [yappu-illakkanam.config :as config]))



(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (css/remove-styles!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (routes/app-routes)
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
