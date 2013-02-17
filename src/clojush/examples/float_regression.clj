;; float_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

;; Differs from simple_regression by using floats and also by precomputing
;; the fitness cases.

(ns clojush.examples.float-regression
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]))

;;;;;;;;;;;;
;; Float symbolic regression of x^3 - 2x^2 - x (problem 5 from the 
;; trivial geography chapter) with minimal float instructions and an 
;; input instruction that uses the auxiliary stack.

(define-registered 
  in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :float state)))

(def fitness-cases
  (for [input (map float (range 10))]
    [input
     (- (* input input input) 
        (* 2.0 input input) 
        input)]))

(define-push-argmap
  :error-function (fn [program]
                    (doall
                      (for [[input target] fitness-cases]
                        (let [state (run-push program 
                                              (push-item input :auxiliary 
                                                         (push-item input :float 
                                                                    (make-push-state))))
                              top-float (top-item :float state)]
                          (if (number? top-float)
                            (Math/abs (- top-float target))
                            1000)))))
  :atom-generators (list (fn [] (rand 10))
                         'in
                         'float_div
                         'float_mult
                         'float_add
                         'float_sub)
  :tournament-size 3)