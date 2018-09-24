(ns yappu-ilakkanam.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [cljss.core :as css]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.routes :as routes]
   [yappu-ilakkanam.appcss :as appcss]
   [yappu-ilakkanam.views :as views]
   [yappu-ilakkanam.config :as config]))



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
  (mount-root)
  (appcss/global-css))
