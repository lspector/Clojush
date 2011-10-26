;; sin_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Tom Helmuth, thelmuth@cs.umass.edu, 2011

(ns examples.sin-regression
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

(import java.lang.Math)

;;;;;;;;;;;;
;; Float symbolic regression of 4*x*sin(4*x) with float instructions and an 
;; input instruction that uses the auxiliary stack.

(define-registered in 
                   (fn [state] (push-item (stack-ref :auxiliary 0 state) :float state)))

(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [input (map #(float (- % 10)) (range 21))]
                        (let [state (run-push program 
                                              (push-item input :auxiliary 
                                                         (push-item input :float 
                                                                    (make-push-state))))
                              top-float (top-item :float state)]
                          (if (number? top-float)
                            (abs (- top-float
                                    (* 4.0 input (Math/sin (* 4.0 input)))))
                            1000.0)))))
  :atom-generators (list (fn [] (float (- (rand-int 21) 10)))
                         'in
                         'float_add
                         'float_sub
                         'float_mult
                         'float_div
                         'float_sin
                         'float_cos
                         'float_tan
                         'float_dup
                         (tag-instruction-erc [:float :exec] 1000)
                         (tagged-instruction-erc 1000))
  :population-size 500
  :max-generations 100
  :tournament-size 7)