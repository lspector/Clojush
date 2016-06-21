;; pagie_hogeweg.clj
;; 
;; Pagie and Hogeweg 1997: Ludo Pagie, Paulien Hogeweg: Evolutionary Consequences of Coevolving Targets. 
;; Evolutionary Computation 5(4): 401-418 (1997). 
;;
;; Kyle Harrington, kyleh@cs.brandeis.edu, 2011

(ns clojush.problems.regression.pagie-hogeweg-with-erc
  (:use [clojush.pushgp.pushgp]
        [clojush pushstate random interpreter])
  (:require [clojure.math.numeric-tower :as math]))

(defn data-point-2D
  "Generate a 2D data point from:
   f(x,y) = 1 / (1 + x^-4) + 1 / ( 1 + y^-4 )"
  [x y]
  (+ (/ (+ 1 (math/expt x -4)))
     (/ (+ 1 (math/expt y -4)))))

(def data (doall (for [x (range -5.0 5.01 0.4) ; Range is exclusive on the end
                       y (range -5.0 5.01 0.4)]
                   [x y (data-point-2D x y)])))

(define-registered 
  x
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :float state)))

(define-registered 
  y
  (fn [state] (push-item (stack-ref :auxiliary 1 state) :float state)))

(defn error-function
  [num-samples program]
  (doall
    (for [row (if (== (count data) num-samples)
                data
                (take num-samples (shuffle data)))]
      (let [state (run-push program 
                            (assoc (make-push-state)
                                   :auxiliary
                                   (butlast row)))
            top-float (top-item :float state)]
        (if (number? top-float)
          (math/abs (- top-float (last row)))
          1000)))))

(defn hit-error-function
  "Error function based on Koza's hit criteria."
  [num-samples program]
  (doall
    (for [row (if (== (count data) num-samples)
                data
                (take num-samples (shuffle data)))]
      (let [state (run-push program 
                            (assoc (make-push-state)
                                   :auxiliary
                                   (butlast row)))
            top-float (top-item :float state)]
        (if (number? top-float)
          (if (< (math/abs (- top-float (last row))) 0.01) 0 1)
          1)))))

(def atom-generators
  (list 
    (fn [] (- (lrand 2) 1))
    'x 'y
    'float_div
    'float_mult
    'float_add
    'float_sub
    ))

(defn problem-specific-report [best population generation sampled-error-function report-simplifications] 
  (let [errors (error-function (count data) (:program best))
        hit-errors (hit-error-function (count data) (:program best))
        total-error (apply + errors)
        hit-total-error (apply + hit-errors)]
    #_(println "Best's errors on full data set:" errors)
    (println "--- Pagie-Hogeweg Problem Specific Report ---")
    (println "Best's total error on full data set:" total-error)
    (println "Best's total error (hits) on full data set:" hit-total-error)
    (if (zero? hit-total-error)
      (assoc best :success true)
      best)))

(def argmap
  {:error-function (partial error-function (count data));; Use all samples
   :atom-generators atom-generators
   :report-simplifications 0
   :max-points 2000
   :max-genome-size-in-initial-program 500
   :evalpush-limit 500
   :population-size 1000
   :max-generations 1000
   :epigenetic-markers []
   :parent-selection :tournament
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.5}
   :uniform-mutation-constant-tweak-rate 0.8
   :uniform-mutation-float-gaussian-standard-deviation 0.01
   :tournament-size 7
   :reuse-errors true ;; If a sample set is used, then error reuse must be disabled
   :print-errors false
   :problem-specific-report problem-specific-report
   })
