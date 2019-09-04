(ns truchet.core
  (:require [reagent.core :as reagent :refer [atom rswap! create-element]]
            [clojure.string :as string]
            [reanimated.core :as anim]
            [tinycolor2 :as tinycolor]
            [goog.string :as gstring]
            [goog.string.format]
            ))

;(def cell-size 100)

;(defn take-cycle
;  ([n xs]
;   (take-cycle n xs 0))
;  ([n xs i]
;   (take n (-> xs cycle (nthrest i)))))

(def rgb-keys {:red :r :green :g :blue :b})

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
  (. js/document (getElementById "app-container")))

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
                  }]
       [:rect {:width w
               :height w
               :fill "none"
               :x (first tl)
               :y (second tl)
               :on-click #(on-click (get params :key))}]])))

(defn grid [params]
  (fn [{:keys [rows cols fill bg cell-data on-cell-click]}]
    [:svg {:class "grid"
           :xmlns "http://www.w3.org/2000/svg"
           :fill fill
           :shape-rendering "crispEdges"
           :pointer-events "all"}
     (let [w (get-in cell-data [[0 0] :w] 0)]
       [:rect {:width (* cols w) :height (* rows w) :fill bg}])
     (doall
      (for [y (range rows) x (range cols)]
           [cell
            (assoc
             (get cell-data [y x])
             :key [y x]
             :fill fill
             :on-click on-cell-click)]))
     ]))

(defn svg-url []
  (let [blob (new js/Blob
                  (array (.. js/document (querySelector "svg.grid") -outerHTML))
                  #js {:type "image/svg+xml"})]
    (. js/URL (createObjectURL blob))))

(defn rgb-slider [{:keys [on-change color-name]}]
  (let [slider {:type "range"
                :min 0
                :max 255
                :onChange on-change
                :step 1}
        numbox (assoc slider :type "number")
        legend (str "Colour " color-name)
        other (if (= color-name "A") "B" "A")
        combined-input
        (fn [n v]
          [:div
           [:label {:for n} n]
           [:span.slider-wrapper [:input (assoc slider :name n :value v)]]
           [:input (assoc numbox :name n :value v)]
           ;[:br]
           ])]
    (fn [{:keys [rgb on-complement-button-click] :as props}]
      (conj
       (into
        [:fieldset [:legend legend]]
        (map
         combined-input
         ["red" "green" "blue"]
         [(rgb :r) (rgb :g) (rgb :b)]))
       [:div [:button.small {:type "button"
                             :name (str "complement" other)
                             :onClick on-complement-button-click}
                             "Complement " other]]))))

(defn rgb-change-handler [a]
  (fn [x]
    (let [target (.. x -currentTarget) ; currentTarget via SyntheticEvent
          k (keyword (. target -name))
          ; input value will always be a valid number or empty string
          ; (see whatwg 4.10.5.1.12) 
          value (js/Number (. target -value))]
      (swap! a assoc (get rgb-keys k) value))))

(defn set-color-from-comp
  ([] nil)
  ([a s]
   (reset! a (js->clj
              (.. (tinycolor (clj->js s)) complement toRgb)
              :keywordize-keys true ))))

(defn button-open-control-menu [{:keys [on-click] :as props}]
  [:button {:onClick on-click :autofocus "" :type "button"} "Controls"])

(defn color-menu [{:keys [fill
                          bg
                          on-swap-button-click
                          on-back-button-click
                          on-fill-rgb-change
                          on-complement-button-click
                          on-bg-rgb-change]}]
  [:div.menu
   [:button.block-display {:onClick on-back-button-click} "Back"]
   [:div.menu-content
    [:form
     [rgb-slider {:rgb fill
                  :on-change on-fill-rgb-change
                  :color-name "A"
                  :on-complement-button-click on-complement-button-click}]
     [rgb-slider {:rgb bg
                  :on-change on-bg-rgb-change
                  :color-name "B"
                  :on-complement-button-click on-complement-button-click}]
     [:div [:button {:type "button" :onClick on-swap-button-click} "Swap"]]]]])

(defn main-menu [props]
  [:div.menu
   [:div
    [:button.block-display {:onClick (props :on-back-button-click)} "Back"]
    [:button.block-display {:onClick (props :on-color-button-click)} "Change colors"]
    [:a.button-link {:href (svg-url) :download ""}
     [:button.block-display {:type "button"} "Save pattern as SVG"]]]])

(defn app []
  (let [rows (atom 0)
        cols (atom 0)
        most-rows (atom 0)
        most-cols (atom 0)
        cell-size (atom 100)
        fill (atom {:r 100 :g 0 :b 200}) 
        bg (atom {:r 255 :g 255 :b 255}) 
        cell-states (atom {})
        control-menu (atom button-open-control-menu)

        ; callbacks
        resize-and-fill-grid
        (fn [{:keys [width height reset-existing?]}]
          (let [cs @cell-size
                new-rows (js/Math.ceil (/ height cs))
                new-cols (js/Math.ceil (/ width cs))
                cell-states-dr @cell-states]
            (->> (concat
                  ; add new rows
                  (for [y (range @most-rows new-rows) x (range new-cols)]
                       (cell-entry {:x x :y y :w cs :r (rand-int 4)}))
                  ; add new columns to existing rows
                  (for [y (range new-rows) x (range @most-cols new-cols)]
                       (cell-entry {:x x :y y :w cs :r (rand-int 4)})))
                 (apply concat)
                 (apply hash-map)
                 (swap! cell-states merge))
            (swap! most-rows max new-rows)
            (swap! most-cols max new-cols)
            (reset! rows new-rows)
            (reset! cols new-cols)))
        on-cell-click
        (fn [k] 
          (let [old-r (get-in @cell-states [k :r])]
            (swap! cell-states assoc-in [k :r] (-> old-r inc (mod 4)))))
        on-fill-rgb-change (rgb-change-handler fill)
        on-bg-rgb-change (rgb-change-handler bg)
        on-swap-button-click
        (fn []
          (let [old-fill @fill
                old-bg @bg]
            (reset! fill old-bg)
            (reset! bg old-fill)))
        on-complement-button-click
        (fn [x]
          (apply set-color-from-comp
                 (condp = (.. x -target -name)
                        "complementB" [fill @bg]
                        "complementA" [bg @fill])))
        close-menu
        (fn [] (reset! control-menu button-open-control-menu))
        open-menu
        (fn [m] (fn [] (reset! control-menu m)))

        control-menu-props
        {button-open-control-menu #(hash-map :on-click (open-menu main-menu))
         main-menu #(hash-map :on-back-button-click close-menu
                              ;:on-save-button-click save-svg
                              :on-color-button-click (open-menu color-menu))
         color-menu #(hash-map :fill @fill
                               :bg @bg
                               :on-complement-button-click on-complement-button-click
                               :on-swap-button-click on-swap-button-click
                               :on-back-button-click (open-menu main-menu)
                               :on-fill-rgb-change on-fill-rgb-change
                               :on-bg-rgb-change on-bg-rgb-change)}]

    ; initialize the component
    (resize-and-fill-grid (get-container-size))
    (. js/window (addEventListener
                  "resize"
                  #(resize-and-fill-grid (get-container-size))))
    ; renderer
    (fn []
      [:div#app
       [:div#menu-wrapper
        [@control-menu ((control-menu-props @control-menu))]]
       [grid {:rows @rows
              :cols @cols
              :svg-size (get-container-size)
              :fill (. (tinycolor (clj->js @fill)) toRgbString)
              :bg (. (tinycolor (clj->js @bg)) toRgbString)
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
