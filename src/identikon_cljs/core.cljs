(ns identikon-cljs.core
  (:require-macros [hiccups.core :as hiccups :refer [html]])
  (:require
   [cljs-hash.goog :as gh]
   [hiccups.runtime :as hiccupsrt]
   [thi.ng.color.core :as col]))

(enable-console-print!)

(def counter (atom 0))

(defn log-str
  ([x]   (do (.log js/console (pr-str x)) x))
  ([m x] (do (log-str {:msg m :data x})   x)))

(defn make-id [nm]
  (let [n (swap! counter inc)]
    (str nm (str n))))

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
  (let [{rwidth :width rheight :height} p
        amt 5
        total (* amt amt)
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

(defn make-hsla [hue]
  "Create a string hex color from a hue, ex: #ff0000"
  (let [hsla (clojure.string/replace "hsla(_,80%,50%,1)" #"_" (str hue))]
    @(-> hsla col/css col/as-int24 col/as-css)))

(hiccups/defhtml make-svg [width height children]
  [:svg {:id (make-id "identikon"), :height height, :width width, :version "1.1",
         :xmlns "https://www.w3.org/2000/svg"} children])

(hiccups/defhtml svg-dot [r cx cy fill]
  [:circle {:id (make-id "iknDot"), :r r, :cx cx, :cy cy, :fill fill}])

(hiccups/defhtml svg-ring [r cx cy stroke stroke-width]
  [:circle {:id (make-id "iknDot"), :r r, :cx cx, :cy cy, :fill "#fff",
            :stroke stroke, :stroke-width stroke-width}])

(defn make-circle-zero [triple xoff roff cr]
  "Large solid circle"
  (let [[x y hue] triple
        r (- cr roff)
        cx (- (+ xoff x) cr)
        cy (- (+ xoff y) cr)
        hsla (make-hsla hue)]
    (svg-dot r cx cy hsla)))

(defn make-circle-one [triple xoff roff cr]
  "Ring"
  (let [[x y hue] triple
        r (- (- cr (/ cr 4)) roff)
        cx (- (+ xoff x) cr)
        cy (- (+ xoff y) cr)
        stroke-width (/ cr 2.5)
        hsla (make-hsla hue)]
    (svg-ring r cx cy hsla stroke-width)))

(defn make-circle-two [triple xoff roff cr]
  "Small solid circle"
  (let [[x y hue] triple
        r (- (- cr (/ cr 3)) roff)
        cx (- (+ xoff x) cr)
        cy (- (+ xoff y) cr)
        hsla (make-hsla hue)]
    (svg-dot r cx cy hsla)))

(defn generate-circle [xoff roff cr dot]
  (let [int (last dot)
        m (mod int 3)]
    (cond
      (= 0 m) (make-circle-zero (take 3 dot) xoff roff cr)
      (= 1 m) (make-circle-one (take 3 dot) xoff roff cr)
      (= 2 m) (make-circle-two (take 3 dot) xoff roff cr))))

(defn insert-svg [svg el]
  (set! (.-innerHTML el) svg))

(defn make-identikon-svg [dimensions]
  "Determine which circle to show by converting the int to mod 3"
  (let [svg (get-svg-attrs dimensions)
        {w :width, h :height, hues :hues, ints :mirror} dimensions
        {coords :coords xoff :xoffset roff :roffset cr :cr} svg
        dot-attrs (map conj coords hues ints)
        dots (map (partial generate-circle xoff roff cr) dot-attrs)]
    (make-svg w h dots)))

(defn ^:export make-identikon [el width height s]
  (let [ints (convert-string s)
        mirror (create-mirror ints)
        [hue-start hue-end] (find-hues ints)
        hues (create-hues (create-hue-range hue-start hue-end 25) 25)
        idk (make-identikon-svg {:mirror mirror, :hues hues, :width width,
                             :height height})]
    (doseq [el (prim-seq (.querySelectorAll js/document el))]
      (insert-svg idk el))))
