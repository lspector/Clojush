(ns clojush.pushgp.visualize
  (:use [clojush.globals])
  (:require [quil.core :as q]))

(def viz-width 1000) 
(def viz-height 1000)
(def viz-border 50)

;; The initial value of viz-data-atom is set set below, in start-visualization.

;; Updates should be made in clojush.pushgp.report/report-and-check-for-success.

(defn visualize []
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
  ;; Initialize viz-data-atom here, with whatever keys you want to track.
  (reset! viz-data-atom {:generation 0
                         :history-of-errors-of-best []})
  (q/sketch
    :size [viz-width viz-height]
    :draw visualize))

(start-visualization)

