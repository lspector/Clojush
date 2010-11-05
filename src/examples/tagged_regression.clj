;; tagged_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns examples.tagged-regression
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the 
;; trivial geography chapter) with minimal integer instructions and an 
;; input instruction that uses the auxiliary stack.
;; ALSO uses tags, although there is little reason to think they would
;; help on such a simple problem.

(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(pushgp 
  :error-function (fn [program]
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
                     'integer_sub
                     (tag-instruction-erc [:integer :exec] 100)
                     (untag-instruction-erc 100)
                     (tagged-instruction-erc 100))
	 :tournament-size 3)