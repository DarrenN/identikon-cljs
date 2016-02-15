(ns identikon-cljs.core
  (:require
   [cljs-hash.goog :as gh]
   [cljsjs.snapsvg]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(defn log-str
  ([x]   (do (.log js/console (pr-str x)) x))
  ([m x] (do (log-str {:msg m :data x})   x)))

(defn sha1 [str]
  (gh/sha1-hex str))

(defn to-pairs [s]
  (map (partial apply str) (partition 2 s)))

(defn to-ints [l]
  (map #(js/parseInt % 16) l))

(defn create-mirror [l]
  "Create a list of ints, break into chunks of 5 and convert those into mirrored chunks so that [0 1 2 3 4] becomes [0 1 2 1 0] and then flatten it all back out"
  (let [h (take 15 l)
        t (reverse (take 10 l))
        f (partition 5 (flatten (cons h t)))
        m (map #(flatten (cons (take 3 %) (reverse (take 2 %)))) f)]
    (flatten m)))

(defn convert-string [s]
  (-> s
      sha1
      to-pairs
      to-ints))

(defn get-svg [id]
  (js/Snap id))

(defn create-hue-range [a b total]
  (let [mn (min a b)
        mx (max a b)]
    (if (< (- mx mn) total)
      (range mn (+ mx (- total (- mx mn))) (/ (- (- total (- mx mn)) mn) 25))
      (range mn mx (/ (- mx mn) 25)))))

(defn create-hues [l total]
  (sort (mapv #(Math/floor (* 1.411 %)) l))) ;; convert 0-255 to 0-360

(defn find-hues [ints]
  (let [remains (drop 15 ints)]
    (vector (first remains) (last remains))))

(defn get-svg-attrs [p]
  "Extract a bunch of attributes from the SVG object and calculate the x,y positions
for all the circles and their radii"
  (let [attr (.attr p)
        amt 5
        total (* amt amt)
        rwidth (aget attr "width") ; original height of SVG
        rheight (aget attr "height") ; original width of SVG
        width (- rwidth (* rwidth .1)) ; create a border offset within SVG
        height (- rheight (* rheight .1))
        xoffset (/ (* rwidth .1) 2) ; amount to offset the dots on x/y
        roffset (/ (* width .1) 12)
        cwidth (Math/floor (/ width amt)) ; circle width
        crad (- (/ cwidth 2) roffset) ; circle radius
        cxs (mapv #(* cwidth %)
                  (drop 1 (take (+ 1 amt) (range)))) ; vector of x coords
        ]
    {
     :width width
     :height height
     :xoffset xoffset
     :roffset roffset
     :cw cwidth
     :cr crad
     :coords (for [x cxs
                   y cxs]
                  [y x])
     }))

(defn make-circle-zero [p triple xoff roff cr]
  "Large solid circle"
  (let [[x y hue] triple
        c (.circle p
                   (- (+ xoff x) cr)
                   (- (+ xoff y) cr)
                   (- cr roff))
        hue (js/Snap.hsl hue 80 50)]
    (.attr c (clj->js {:fill hue}))))

(defn make-circle-one [p triple xoff roff cr]
  "Ring"
  (let [[x y hue] triple
        c (.circle p
                   (- (+ xoff x) cr)
                   (- (+ xoff y) cr)
                   (- (- cr (/ cr 4)) roff))
        hue (js/Snap.hsl hue 80 50)]
    (.attr c (clj->js {:fill "#ffffff" :stroke hue :strokeWidth (/ cr 2)}))))

(defn make-circle-two [p triple xoff roff cr]
  "Small solid circle"
  (let [[x y hue] triple
        c (.circle p
                   (- (+ xoff x) cr)
                   (- (+ xoff y) cr)
                   (- (- cr (/ cr 3)) roff))
        hue (js/Snap.hsl hue 80 50)]
    (.attr c (clj->js {:fill hue}))))

(defn make-identikon [p ints hues]
  "Determine which circle to show by converting the int to mod 3"
  (let [svg (get-svg-attrs p)
        {coords :coords xoff :xoffset roff :roffset cr :cr} svg]
    (doseq [x (map conj coords hues ints)]
      (let [int (last x)
            m (mod int 3)]
        (log-str x)
        (cond
          (= 0 m) (make-circle-zero p (take 3 x) xoff roff cr)
          (= 1 m) (make-circle-one p (take 3 x) xoff roff cr)
          (= 2 m) (make-circle-two p (take 3 x) xoff roff cr))))))

(defn make [id s]
  (let [paper (get-svg id)
        ints (convert-string s)
        mirror (create-mirror ints)
        [hue-start hue-end] (find-hues ints)
        hues (create-hues (create-hue-range hue-start hue-end 25) 25)]
    (make-identikon paper mirror hues)))
