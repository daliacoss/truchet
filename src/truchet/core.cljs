(ns truchet.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]
            [reanimated.core :as anim]
            [goog.string :as gstring]
            [goog.string.format]
            ))

(defonce app-state (atom {:text "Hello world!"}))

(def cell-size 100)

(defn take-cycle
  ([n xs]
   (take-cycle n xs 0))
  ([n xs i]
   (take n (-> xs cycle (nthrest i)))))

(defn cell [{:keys [x y r w]
             :or {x 0, y 0, r 0, w cell-size}}]
  (let [tl [(* x w) (* y w)]
        tr (update tl 0 + w)
        bl (update tl 1 + w)
        br (update tr 1 + w)
        r (atom r)
        points (map atom (repeat 6 0))
        anim-points (map anim/interpolate-to points)]
    (fn []
      (dorun (map reset! points (apply concat (take-cycle 3 [tl tr br bl] @r))))
      [:g
       [:polygon {:points (string/join " " (map deref anim-points))}]
       [:rect {:width w
               :height w
               :fill "none"
               :x (first tl)
               :y (second tl)
               :on-click #(swap! r inc)}]])))

(defn grid [{:keys [width height]}]
  [:svg {:width "100%" :height "100%" :pointer-events "all"}
   (let [rows (js/Math.ceil (/ height cell-size))
         cols (js/Math.ceil (/ width cell-size))]
     (for [y (range rows) x (range cols)]
        [cell {:r (rand-int 4) :x x :y y :key (gstring/format"%d-%d" y x)}]))
   ])

(defn render-grid []
  (let [container (. js/document (getElementById "app"))
        props {:width container.clientWidth :height container.clientHeight}]
    (reagent/render-component [grid props] container)))

(defn handle-keyup [x]
  (if (= x.key " ") (render-grid)))

(defn start []
  (. js/document (addEventListener "keyup" handle-keyup))
  (render-grid))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
