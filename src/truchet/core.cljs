(ns truchet.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]
            [goog.string :as gstring]
            [goog.string.format]
            ))

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn take-cycle
  ([n xs]
   (take-cycle n xs 0))
  ([n xs i]
   (take n (-> xs cycle (nthrest i)))))

(defn cell [{:keys [x y r] :or {x 0 y 0 r 0}}]
  (let [w 50
        tl [(* x w) (* y w)]
        tr (update tl 0 + w)
        bl (update tl 1 + w)
        br (update tr 1 + w)
        points (apply concat (take-cycle 3 [tl tr br bl] r))]
    [:polygon {:points (string/join " " points)}]))

(defn hello-world []
  [:svg {:width "100%" :height 500}
   (for [y (range 10) x (range 10)]
     [cell {:r (rand-int 3) :x x :y y :key (gstring/format"%d-%d" y x)}])
   ])

(defn start []
  (reagent/render-component [hello-world]
                            (. js/document (getElementById "app"))))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
