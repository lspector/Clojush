(ns clojush.examples.argmap-regression
  (:use [clojush.pushstate]
        [clojush.interpreter]
        [clojush.experimental.pushgp-map]
        [clojure.math.numeric-tower]))

;; argmap_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

;; This is the same as simple_regression except passes all of its arguments 
;; in a map. This is just a demo of pushgp-map.

;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the 
;; trivial geography chapter) with minimal integer instructions and an 
;; input instruction that uses the auxiliary stack.

(define-registered in 
                   (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(pushgp-map
  {:error-function (fn [program]
                     (doall
                       (for [input (range 10)]
                         (let [state (run-push program 
                                               (push-item input :auxiliary 
                                                          (push-item input :integer 
                                                                     (make-push-state))))
                               top-int (top-item :integer state)]
                           (if (number? top-int)
                             (abs (- top-int 
                                     (- (* input input input) 
                                        (* 2 input input) input)))
                             1000)))))
   :atom-generators (list (fn [] (rand-int 10))
                          'in
                          'integer_div
                          'integer_mult
                          'integer_add
                          'integer_sub)
   :tournament-size 3})
