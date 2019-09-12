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

(defn cell-coords [{:keys [col row r size]}]
  (let [tl [(* col size) (* row size)]
        tr (update tl 0 + size)
        bl (update tl 1 + size)
        br (update tr 1 + size)]
    (case r
      0 [tl tr br]
      1 [tr br bl]
      2 [br bl tl]
      3 [bl tl tr]
      nil)))

(defn cell [props]
  (let [points (map atom (apply concat (cell-coords props)))
        anim-points (map anim/interpolate-to points)]
    (fn [{:keys [r size col row on-click] :as props}]
      (dorun
       ; flatten vector of vectors
       (map reset! points (apply concat (cell-coords props))))
      [:g {:data-key (get props :key)}
       [:polygon {:points (string/join " " (map deref anim-points))}]
       [:rect {:width size
               :height size
               :fill "none"
               :x (* col size)
               :y (* row size)
               :on-click #(on-click [row col])
               ;:on-click on-click
               }]])))

(defn grid [params]
  (fn [{:keys [rows cols cell-size fill bg cell-data on-cell-click]}]
    [:svg {:class "grid"
           :xmlns "http://www.w3.org/2000/svg"
           :fill fill
           :shape-rendering "crispEdges"
           :pointer-events "all"}
     [:rect {:width (* cols cell-size) :height (* rows cell-size) :fill bg}]
     (doall
      (for [y (range rows) x (range cols)]
           [cell
            (assoc
             (get cell-data [y x])
             :size cell-size
             :col x
             :row y
             :key [y x cell-size]
             :fill fill
             :on-click on-cell-click)]))
     ]))

(defn svg-url []
  (let [blob (new js/Blob
                  (array (.. js/document (querySelector "svg.grid") -outerHTML))
                  #js {:type "image/svg+xml"})]
    (. js/URL (createObjectURL blob))))

(defn slider-with-num-field [props]
  (let [slider (conj {:type "range"
                      :min 0
                      :value 0
                      :name "my-field"
                      :step 1
                      ; allow either on-change or onChange
                      :onChange (props :on-change)}
                     props)
        my-name (get slider :name)
        numbox (assoc slider :type "number")]
    [:div
     [:label {:for my-name} (get props :label my-name)]
     [:span.slider-wrapper [:input slider]]
     [:input numbox]]))

(defn rgb-fieldset [{:keys [rgb
                            on-complement-button-click
                            on-change
                            color-name]}]
  (let [other (if (= color-name "A") "B" "A")]
    (conj
     (into
      [:fieldset [:legend (str "Colour " color-name)]]
      (map
       (fn [n v]
         [slider-with-num-field {:max 255
                                 :onChange on-change
                                 :name n
                                 :value v}])
       ["red" "green" "blue"]
       [(rgb :r) (rgb :g) (rgb :b)]))
     [:div [:button.small {:type "button"
                           :name (str "complement" other)
                           :onClick on-complement-button-click}
                           "Complement " other]])))

(defn rgb-change-handler [color-atom]
  (fn [x]
    (let [target (.. x -currentTarget) ; currentTarget via SyntheticEvent
          k (keyword (. target -name))
          ; input value will always be a valid number or empty string
          ; (see whatwg 4.10.5.1.12) 
          value (js/Number (. target -value))]
      (swap! color-atom assoc (get rgb-keys k) value))))

(defn color-swap-handler [a-atom b-atom]
  (fn []
    (let [old-a @a-atom
          old-b @b-atom]
      (reset! a-atom old-b)
      (reset! b-atom old-a))))

(defn zoom-change-handler [cell-size-atom resize]
  (fn [x]
    (let [value (js/Number (.. x -currentTarget -value))]
      (reset! cell-size-atom value)
      (resize (get-container-size))
      )))

(defn cell-click-handler [cell-states]
  (fn [k] 
    (let [old-r (get-in @cell-states [k :r])]
      (swap! cell-states assoc-in [k :r] (-> old-r inc (mod 4))))))

;(defn -cell-click-handler [cell-states]
;  (fn [x] 
;    (let [g (.. x -target -parentElement)
;          col (js/Number (. g getAttribute "data-col"))
;          row (js/Number (. g getAttribute "data-row"))
;          k [row col] 
;          old-r (get-in @cell-states [k :r])]
;      (swap! cell-states assoc-in [k :r] (-> old-r inc (mod 4))))))

(defn set-color-from-comp
  ([] nil)
  ([a s]
   (reset! a (js->clj
              (.. (tinycolor (clj->js s)) complement toRgb)
              :keywordize-keys true ))))

(defn button-open-control-menu [{:keys [on-click] :as props}]
  [:button {:onClick on-click :autofocus "" :type "button"} "Controls"])

(defn zoom-menu [{:keys [cell-size on-back-button-click on-zoom-change]}]
  [:div.menu
   [:button.block-display {:onClick on-back-button-click} "Back"]
   [:div.menu-content
    [:form
     [slider-with-num-field {:min 40
                             :max 200
                             :on-change on-zoom-change
                             :name "zoom"
                             :label "tile length (pixels)"
                             :value cell-size}]]]])

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
     [rgb-fieldset {:rgb fill
                    :on-change on-fill-rgb-change
                    :color-name "A"
                    :on-complement-button-click on-complement-button-click}]
     [rgb-fieldset {:rgb bg
                    :on-change on-bg-rgb-change
                    :color-name "B"
                    :on-complement-button-click on-complement-button-click}]
     [:div [:button {:type "button" :onClick on-swap-button-click} "Swap"]]]]])

(defn button-save-svg []
  [:a.button-link {:href (svg-url) :download ""}
   [:button.block-display {:type "button"} "Save pattern as SVG"]])

(defn str->num-vec [s]
  (->> #","
       (string/split s)
       (map js/Number)
       vec))

(defn keywordize-keys [coll]
  (zipmap (map keyword (keys coll)) (vals coll)))

(defn get-from-storage [k fallback]
  (or
   (if-let [v (js->clj (. js/JSON parse (.. js/window -localStorage (getItem k))))]
     (if (= (name k) "cell-states")
      (zipmap (map str->num-vec (keys v)) (map keywordize-keys (vals v)))
      v)
   fallback)))

(defn save-to-storage
  ([k v]
   (let [v (if (= k :cell-states)
               ; turn cell-state keys (e.g., [0 1]) into strings (e.g., "0,1")
               (zipmap (map #(string/join "," %) (keys v)) (vals v))
               v)
         stringified-v (. js/JSON (stringify (clj->js v)))]
     (.. js/window -localStorage (setItem (name k) stringified-v))))
  ([k v & more]
   (->> more
        (concat [k v])
        (partition 2)
        (map #(apply save-to-storage %))
        dorun)))
   ;(dorun (map save-to-storage (partition 2 (concat [k v] more))))))

(defn main-menu [{:keys [items]}]
  [:div.menu 
   (conj
    (->> items
         (map #(vector :button.block-display
                       {:onClick (get % :on-click)}
                       (get % :name)))
         (into [:div]))
    [button-save-svg]
    )])

(defn app []
  (let [rows (atom 0)
        cols (atom 0)
        most-rows (atom (get-from-storage "most-rows" 0))
        most-cols (atom (get-from-storage "most-cols" 0))
        cell-size (atom (get-from-storage "cell-size" 100))
        fill (atom (get-from-storage "fill" {:r 0 :g 0 :b 0}))
        bg (atom (get-from-storage "bg" {:r 255 :g 255 :b 255}))
        cell-states (atom (get-from-storage "cell-states" {}))
        control-menu (atom button-open-control-menu)
        update-since-last-save? (atom false)

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
                       ;(cell-entry {:x x :y y :w cs :r (rand-int 4)}))
                       [[y x] {:r (rand-int 4)}])
                  ; add new columns to existing rows
                  (for [y (range new-rows) x (range @most-cols new-cols)]
                       ;(cell-entry {:x x :y y :w cs :r (rand-int 4)})))
                       [[y x] {:r (rand-int 4)}]))
                 (apply concat)
                 (apply hash-map)
                 (swap! cell-states merge))
            (swap! most-rows max new-rows)
            (swap! most-cols max new-cols)
            (reset! rows new-rows)
            (reset! cols new-cols)))
        on-cell-click (cell-click-handler cell-states)
        on-fill-rgb-change (rgb-change-handler fill)
        on-bg-rgb-change (rgb-change-handler bg)
        on-swap-button-click (color-swap-handler fill bg)
        on-zoom-change (zoom-change-handler cell-size resize-and-fill-grid)
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
         main-menu #(hash-map :items [{:on-click close-menu
                                       :name "Back"}
                                      {:on-click (open-menu color-menu)
                                       :name "Change colors"}
                                      {:on-click (open-menu zoom-menu)
                                       :name "Change zoom level"}])
         zoom-menu #(hash-map :on-back-button-click (open-menu main-menu)
                              :on-zoom-change on-zoom-change
                              :cell-size @cell-size)
         color-menu #(hash-map :fill @fill
                               :bg @bg
                               :on-complement-button-click on-complement-button-click
                               :on-swap-button-click on-swap-button-click
                               :on-back-button-click (open-menu main-menu)
                               :on-fill-rgb-change on-fill-rgb-change
                               :on-bg-rgb-change on-bg-rgb-change)}]

    ; add watches
    (let [f #(reset! update-since-last-save? true)]
      (dorun (map #(add-watch % :track-update f)
                  [most-rows, most-cols, cell-states, fill, bg, cell-size])))

    ; initialize the component
    (resize-and-fill-grid (get-container-size))
    (. js/window (addEventListener
                  "resize"
                  #(resize-and-fill-grid (get-container-size))))
    (js/setInterval
     (fn []
       (if update-since-last-save? 
        (do
          (save-to-storage :most-rows @most-rows
                           :most-cols @most-cols
                           :cell-states @cell-states
                           :fill @fill
                           :bg @bg
                           :cell-size @cell-size)
          (reset! update-since-last-save? false))))
     5000)

    ; renderer
    (fn []
      [:div#app
       [:div#menu-wrapper
        [@control-menu ((control-menu-props @control-menu))]]
       [grid {:rows @rows
              :cols @cols
              :cell-size @cell-size
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
