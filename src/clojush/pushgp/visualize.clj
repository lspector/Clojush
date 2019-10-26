(ns clojush.pushgp.visualize
  (:use [clojush.globals])
  (:require [quil.core :as q]))

;; This file demonstrates how one can produce a visualization of the status of a 
;; pushgp run as it proceeds. It can be invoked by specifying ":visualize true"
;; in the argument map, or in a command line such as:

;; lein run clojush.problems.demos.simple-regression :parent-selection :lexicase :visualize true

;; The default behavior is not intended to be particularly general or useful, but 
;; just to provide a demonstration that can serve as a base upon which more general
;; visualization systems or specialized visualizations for particular problems and
;; research questions can be built. 

;; That said, the default behavior is to plot all of the errors (for all test cases, 
;; individually) on the y axis, for the best (lowest total error) individual in each 
;; generation, with generations running across the x axis. Errors are plotted as
;; colored circles, with colors chosen arbitrarily to span the range of hues in a
;; hue-saturation-brightness color encoding. Transparency is used to make overlaps
;; visible to some extent. The x and y axes are scaled dynamically over the run.

;; The system works by launching a Quil/Processing (http://quil.info) "sketch" and
;; having that sketch run concurrently with the pushgp run. A Clojure atom, called
;; viz-data-atom (created in clojush.globals), is used to communicate between pushgp
;; and the Quil sketch.

;; The initial value of viz-data-atom is set set below, in start-visualization.
;; By default it contains a map with keys only for :generation and 
;; :history-of-errors-of-best, but this can be extended as you wish.

;; Updates to viz-data-atom should be made in report-and-check-for-success, in
;; clojush.pushgp.report -- search for viz-data-atom in src/clojush/pushgp/report.clj
;; to find the code to customize there.

;; The actual visualization code is in the visualize function below, and should use
;; Quil functions to render the infromation in viz-data-atom.

(def viz-width 1000) ;; width of the visualization window in pixels
(def viz-height 1000) ;; height of the visualization window in pixels
(def viz-border 50) ;; pixels from centers of edge points to window edge

(defn visualize []
  "Render the information in viz-data-atom to the visualization window."
  (let [data @viz-data-atom
        gen (:generation data)]
    (when (> gen 0)
      (let [bests (:history-of-errors-of-best data)
            num-errors (count (first bests))
            scale-x (/ (- viz-width (* 2 viz-border))
                       (max 1 gen))
            scale-y (/ (- viz-height (* 2 viz-border))
                       (max 1 (apply max (flatten bests))))]
        (q/color-mode :rgb 255)
        (q/background 255)
        ;; produce text in the window with code such as the following
        ;(q/fill 0)
        ;(q/text (str "Generation: " gen) 10 20)
        (q/color-mode :hsb)
        (doseq [[g errs] (map vector (iterate inc 0) bests)]
          (doseq [[i e] (map vector (iterate inc 0) errs)]
            (q/no-stroke)
            (q/fill (* i (/ 255 num-errors)) 255 255 64)
            (q/ellipse (+ viz-border (* g scale-x))
                       (- viz-height viz-border (* e scale-y)) 
                       20
                       20)))))))

(defn start-visualization []
  "Initialize viz-data-atom and create the Quil sketch."
  (reset! viz-data-atom {:generation 0
                         :history-of-errors-of-best []})
  (q/sketch
    :size [viz-width viz-height]
    :draw visualize))

;; Start the visualization when this file is loaded.
(start-visualization)

