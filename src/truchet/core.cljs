(ns truchet.core
  (:require [reagent.core :as reagent :refer [atom rswap!]]
            [clojure.string :as string]
            [reanimated.core :as anim]
            [goog.string :as gstring]
            [goog.string.format]
            ))

(def cell-size 100)

;(defn take-cycle
;  ([n xs]
;   (take-cycle n xs 0))
;  ([n xs i]
;   (take n (-> xs cycle (nthrest i)))))

(defn cell-entry [{:keys [x y w r]}]
  (let [tl [(* x w) (* y w)]
        tr (update tl 0 + w)
        bl (update tl 1 + w)
        br (update tr 1 + w)]
    [[y x]
     {:r r
      :tl tl
      :w w
      :coords [[tl tr br]
               [tr br bl]
               [br bl tl]
               [bl tl tr]]}]))

(defn get-container []
  (. js/document (getElementById "app")))

(defn get-container-size []
  (let [c (get-container)]
    {:width c.clientWidth :height c.clientHeight}))

(defn cell [{:keys [r coords]}]
  (let [points (map atom (apply concat (get coords r)))
        anim-points (map anim/interpolate-to points)]
    (fn [{:keys [r coords tl w on-click] :as params}]
      (dorun
       ; flatten vector of vectors
       (map reset! points (apply concat (get coords r))))
      [:g
       [:polygon {:points (string/join " " (map deref anim-points))
                  :fill "black"}]
       [:rect {:width w
               :height w
               :fill "none"
               :x (first tl)
               :y (second tl)
               :on-click #(on-click (get params :key))}]])))

(defn grid [params]
  (fn [{:keys [rows cols cell-data on-cell-click]}]
    [:svg {:width "100%" :height "100%" :pointer-events "all"}
     (doall
      (for [y (range rows) x (range cols)]
           [cell
            (assoc
             (get cell-data [y x])
             :key [y x]
             :on-click on-cell-click)]))
     ]))

(defn app []
  ;(let [container (. js/document (getElementById "app"))
  ;      width container.clientWidth
  ;      height container.clientHeight
  ;      rows (js/Math.ceil (/ height cell-size))
  ;      cols (js/Math.ceil (/ width cell-size))]
  (let [rows (atom 0)
        cols (atom 0)
        cell-states (atom {})

        resize-and-fill-grid
        (fn [{:keys [width height]}]
          (let [new-rows (js/Math.ceil (/ height cell-size))
                new-cols (js/Math.ceil (/ width cell-size))
                cell-states-dr @cell-states]
            (->> (concat
                  ; add new rows
                  (for [y (range @rows new-rows) x (range new-cols)]
                       (cell-entry {:x x :y y :w cell-size :r (rand-int 4)}))
                  ; add new columns to existing rows
                  (for [y (range new-rows) x (range @cols new-cols)]
                       (cell-entry {:x x :y y :w cell-size :r (rand-int 4)})))
                 (apply concat)
                 (apply hash-map)
                 (swap! cell-states merge))
            (reset! rows new-rows)
            (reset! cols new-cols)))

        on-cell-click
        (fn [k] 
          (let [old-r (get-in @cell-states [k :r])]
            (swap! cell-states assoc-in [k :r] (-> old-r inc (mod 4)))))]

    (resize-and-fill-grid (get-container-size))
    (. js/window (addEventListener
                  "resize"
                  #(resize-and-fill-grid (get-container-size))))
    (fn []
      [grid {:rows @rows
             :cols @cols
             :on-cell-click on-cell-click
             :cell-data @cell-states}])))

(defn start []
  (reagent/render-component [app] (get-container)))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
