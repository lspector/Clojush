(ns clojush.pushgp.visualize
  (:use [clojush.globals])
  (:require [quil.core :as q]))

;; The initial value of viz-data-atom is set in clojush.pushgp.report/initial-report,
;; and updates are made in other functions in that namespace.

(defn visualize []
  (let [scale-x 10
        scale-y 2]
    (q/background 255)
    (q/fill 0)
    (q/text "Evolution!" 100 100)
    (let [gen (:generation @viz-data-atom)]
      (q/line (* (inc gen) scale-x) 0 (* (inc gen) scale-x) 1000))
    (doseq [[gen err] 
            (map vector 
                 (iterate inc 0) 
                 (map (partial * scale-y) 
                      (:best-total-error-history @viz-data-atom)))]
      (q/fill 0)
      (q/rect (* gen scale-x) (- 1000 (* err scale-y) scale-y) scale-x scale-y))))

(defn start-visualization []
  (q/sketch
    :size [1000 1000]
    :draw visualize))

(start-visualization)

