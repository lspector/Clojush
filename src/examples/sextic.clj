;; sextic.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns examples.sextic
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

;;;;;;;;;;;;
;; Floating point symbolic regression of the "sextic polynomial" y=x^6-2x^4+x^2. This uses
;; the core float arithmetic instructions and an input instruction that uses the auxiliary stack.
;; This example also demonstrates  the use of fitness penalties for abnormally terminating programs.


(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :float state)))

(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [input (range -1.0 1.0 0.1)]
                        (let [state (run-push program 
                                      (push-item input :auxiliary 
                                        (push-item input :float
                                          (make-push-state))))
                              top-float (top-item :float state)
                              invalid-output (or (not (number? top-float))
                                               (= (:termination state) :abnormal))]
                          (if invalid-output
                            1000
                            (abs (- top-float
                                   (+ (* input input input input input input)
                                     (- (* 2 input input input input))
                                     (* input input)))))))))
	 :atom-generators (concat 
                     '(float_div float_mult float_sub float_add
                        float_rot float_swap float_dup float_pop)
                     (list 
                       (fn [] (* 1.0 (- (rand-int 21) 10)))
                       'in))
	 :population-size 10000)
