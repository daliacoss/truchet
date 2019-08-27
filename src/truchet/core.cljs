(ns truchet.core
  (:require [reagent.core :as reagent :refer [atom rswap! create-element]]
            [clojure.string :as string]
            [reanimated.core :as anim]
            [tinycolor2 :as tinycolor]
            [react-color]
            [goog.string :as gstring]
            [goog.string.format]
            ))

;(def cell-size 100)

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
                  :fill (get params :fill "black")}]
       [:rect {:width w
               :height w
               :fill "none"
               :x (first tl)
               :y (second tl)
               :on-click #(on-click (get params :key))}]])))

(defn grid [params]
  (fn [{:keys [rows cols fill cell-data on-cell-click]}]
    [:svg {:width "100%" :height "100%" :pointer-events "all"}
     (doall
      (for [y (range rows) x (range cols)]
           [cell
            (assoc
             (get cell-data [y x])
             :key [y x]
             :fill fill
             :on-click on-cell-click)]))
     ]))


(defn rgb-slider [{:keys [on-change]}]
  (let [slider {:type "range"
                :min 0
                :max 255
                :onChange on-change
                :step 1
                :class "color-slider"}]
    (fn [{:keys [rgb] :as props}]
      [:form
       [:input (assoc slider :name "red" :value (rgb :r))][:br]
       [:input (assoc slider :name "green" :value (rgb :g))][:br]
       [:input (assoc slider :name "blue" :value (rgb :b))]])))

(def rgb-keys {:red :r :green :g :blue :b})

(defn app []
  (let [rows (atom 0)
        cols (atom 0)
        cell-size (atom 100)
        fill (atom {:r 100 :g 0 :b 200}) 
        cell-states (atom {})

        ; callbacks
        resize-and-fill-grid
        (fn [{:keys [width height reset-existing?]}]
          (let [cs @cell-size
                new-rows (js/Math.ceil (/ height cs))
                new-cols (js/Math.ceil (/ width cs))
                cell-states-dr @cell-states]
            (->> (concat
                  ; add new rows
                  (for [y (range @rows new-rows) x (range new-cols)]
                       (cell-entry {:x x :y y :w cs :r (rand-int 4)}))
                  ; add new columns to existing rows
                  (for [y (range new-rows) x (range @cols new-cols)]
                       (cell-entry {:x x :y y :w cs :r (rand-int 4)})))
                 (apply concat)
                 (apply hash-map)
                 (swap! cell-states merge))
            (reset! rows new-rows)
            (reset! cols new-cols)))
        on-cell-click
        (fn [k] 
          (let [old-r (get-in @cell-states [k :r])]
            (swap! cell-states assoc-in [k :r] (-> old-r inc (mod 4)))))
        on-rgb-change
        (fn [x]
          (let [target (.. x -currentTarget)
                k (keyword (. target -name))
                value (. target -value)]
            (swap! fill assoc (get rgb-keys k) value)))] ; see react's SyntheticEvent

    ; initialize the component
    (resize-and-fill-grid (get-container-size))
    (. js/window (addEventListener
                  "resize"
                  #(resize-and-fill-grid (get-container-size))))
    ; renderer
    (fn []
      [:span
       [:div.menu
        [:button "settings"]
        [:div.color-form
         ;[rgb-slider {:rgb @fill :on-change on-rgb-change}]
         [rgb-slider {:rgb @fill :on-change on-rgb-change}]]]
       [grid {:rows @rows
              :cols @cols
              :fill (. (tinycolor (clj->js @fill)) toRgbString)
              :on-cell-click on-cell-click
              :cell-data @cell-states}]])))

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
