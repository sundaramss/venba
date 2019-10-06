(ns yappu-ilakkanam.routes
  (:require [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [clojure.string :as str]
            [re-frame.core :refer [dispatch]]
            [yappu-ilakkanam.events :as events]))

(defn set-hash! [loc]
  (set! (.-hash js/window.location) loc)
  [])

(defn- parse-path [path]
  (let [spath (re-seq #"((\d+)/([^/]+))" (or path ""))]
   (reduce
     (fn [r x] (let [[p q] (subvec x 2)] (conj r {:id (js/parseInt p) :opt q :frecords nil})))
     [] spath)))

(defn- get-pageno [path]
  (if-let [pageno (second (re-matches #".*[&\?]p=(\d+)" path))]
     (js/parseInt pageno)
     1))

(def routes
  ["/" [["" :home]
        [[:padal "/" [#"\d+" :pageno]] :pa]
        ;[[:padal "/s/" [#"((\d+)/([^/]+))" :path]] :pasearch]]])
        [[:padal "/s/" [#".*" :path]] :pasearch]]])

(def history
  (let [dispatch (fn [request]
                   (let [{handler :handler} request
                         {padal :padal pageno :pageno path :path} (:route-params request)
                         paths (parse-path path)
                         pagenumber (when pageno (js/parseInt pageno))
                         id (or pagenumber (get-in request [:query-params :pageno] 1))]
                      (if (= :home handler) (set-hash! "/kural/1")
                        (when (and handler padal)
                          (dispatch
                            [::events/set-active-page {:page handler :id id :padal padal :paths paths}])))))
        match (fn [req]
                 (let [path (subs js/window.location.hash 1)
                       newpath (if (str/blank? path) "/" path)
                       pageno (get-pageno path)]
                   (-> (bidi/match-route routes newpath)
                       (assoc :query-params {:pageno pageno}))))]
    (pushy/pushy dispatch match)))

;; -- Router Start ------------------------------------------------------------
(defn start!
  []
  (pushy/start! history))

;; -- url-for -----------------------------------------------------------------
(def url-for (partial bidi/path-for routes))

(defn set-token!
  [token]
  (pushy/set-token! history token))
