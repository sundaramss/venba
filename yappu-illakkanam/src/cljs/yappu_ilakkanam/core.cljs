(ns yappu-ilakkanam.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [yappu-ilakkanam.events :as events]
   [yappu-ilakkanam.routes :as routes]
   [yappu-ilakkanam.views :as views]
   [yappu-ilakkanam.config :as config]))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn init []
  (routes/start!)
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
