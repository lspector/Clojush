;; odd.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns examples.odd
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

;;;;;;;;;;;;
;; The "odd" problem: take a positive integer input and push a Boolean indicating
;; whether or not the input is an odd number. There are many ways to compute this
;; and PushGP sometimes finds unusual methods.

(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [input (range 10)]
                        (let [state (run-push program
                                      (push-item input :auxiliary
                                        (push-item input :integer
                                          (make-push-state))) )
                              top-bool (top-item :boolean state)]
                          (if (not (= top-bool :no-stack-item))
                            (if (= top-bool (odd? input)) 0 1)
                            1000)))))
  :atom-generators (concat (registered-nonrandom)
                     (list (fn [] (rand-int 100))
                       'in)))