(ns identikon-cljs.core
  (:require
   [cljs-hash.goog :as gh]
   [thi.ng.color.core :as col]
   [cljsjs.svgjs]))

(enable-console-print!)

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
  (let [t (reverse (take 5 l))
        f (partition 5 (flatten (cons l t)))
        m (map #(flatten (cons (take 3 %) (reverse (take 2 %)))) f)]
    (flatten m)))

(defn convert-string [s]
  "Turn the hash string into a collections of integers (0-255)"
  (-> s
      sha1
      to-pairs
      to-ints))

(defn get-svg [id]
  "SVG.js converts an SVG to 100% x 100% on grabbing the DOM element so we force it back to its original dimensions and pass them back with the element"
  (let [d (.getElementById js/document id)
        w (js/parseInt (.getAttribute d "width") 10)
        h (js/parseInt (.getAttribute d "height") 10)
        p (js/SVG id)]
    (.spof p)
    (.attr p "width" w)
    (.attr p "height" h)
    {:svg p :width w :height h}))

(defn create-hue-range [a b total]
  (let [mn (min a b)
        mx (max a b)
        d (- mx mn)]
    (range mn mx (/ d total))))

(defn create-hues [l total]
  (sort (mapv #(Math/floor (* 1.411 %)) l))) ;; convert 0-255 to 0-360

(defn find-hues [ints]
  "Add up the first 3 and last 3 integers of the last 5 to generate a start and end range for the hues (0-255)"
  (let [remains (drop 15 ints)
        h (take 3 remains)
        t (drop 2 remains)]
    (vector (mod (apply + h) 255) (mod (apply + t) 255))))

(defn get-svg-attrs [p]
  "Use the width and height of the SVG object to calculate the x,y positions for all the circles and their radii"
  (let [{svg :svg rwidth :width rheight :height} p
        attr (.attr svg)
        amt 5
        total (* amt amt)
        width (- rwidth (* rwidth .1)) ; create a border offset within SVG
        height (- rheight (* rheight .1))
        xoffset (/ (* rwidth .1) 2) ; amount to offset the dots on x/y
        roffset (/ (* width .1) 12)
        cwidth (Math/floor (/ width amt)) ; circle width
        crad (- (* cwidth .85) roffset) ; circle radius
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

(defn make-hsla [hue]
  "Create a string hex color from a hue, ex: #ff0000"
  (let [hsla (clojure.string/replace "hsla(_,80%,50%,1)" #"_" (str hue))]
    @(-> hsla col/css col/as-int24 col/as-css)))

(defn make-circle-zero [p triple xoff roff cr]
  "Large solid circle"
  (let [[x y hue] triple
        c (.circle p (- cr roff))
        hsla (make-hsla hue)]
    (.attr c (clj->js {:cx (- (+ xoff x) cr)
                       :cy (- (+ xoff y) cr)
                       :fill hsla}))))

(defn make-circle-one [p triple xoff roff cr]
  "Ring"
  (let [[x y hue] triple
        c (.circle p (- (- cr (/ cr 4)) roff))
        hsla (make-hsla hue)]
    (.attr c (clj->js {:cx (- (+ xoff x) cr)
                       :cy (- (+ xoff y) cr)
                       :fill "#fff"
                       :stroke-width (/ cr 4)
                       :stroke hsla}))))

(defn make-circle-two [p triple xoff roff cr]
  "Small solid circle"
  (let [[x y hue] triple
        c (.circle p (- (- cr (/ cr 3)) roff))
        hsla (make-hsla hue)]
    (.attr c (clj->js {:cx (- (+ xoff x) cr)
                       :cy (- (+ xoff y) cr)
                       :fill hsla}))))

(defn make-identikon [p ints hues]
  "Determine which circle to show by converting the int to mod 3"
  (let [svg (get-svg-attrs p)
        {paper :svg} p
        {coords :coords xoff :xoffset roff :roffset cr :cr} svg]
    (doseq [x (map conj coords hues ints)]
      (let [int (last x)
            m (mod int 3)]
        (cond
          (= 0 m) (make-circle-zero paper (take 3 x) xoff roff cr)
          (= 1 m) (make-circle-one paper (take 3 x) xoff roff cr)
          (= 2 m) (make-circle-two paper (take 3 x) xoff roff cr))))))

(defn ^:export make [id s]
  (let [paper (get-svg id)
        ints (convert-string s)
        mirror (create-mirror ints)
        [hue-start hue-end] (find-hues ints)
        hues (create-hues (create-hue-range hue-start hue-end 25) 25)]
    (make-identikon paper mirror hues)))
