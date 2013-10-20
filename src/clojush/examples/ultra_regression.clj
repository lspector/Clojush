;; ultra_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2013

(ns clojush.examples.ultra-regression
  (:use clojush.pushgp.pushgp)
  (:require clojush.examples.simple-regression))

;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the 
;; trivial geography chapter) with minimal integer instructions and an 
;; input instruction that uses the auxiliary stack.
;; Uses only the ULTRA genetic operator.

(def argmap
  (merge clojush.examples.simple-regression/argmap
         {:mutation-probability 0.0
          :crossover-probability 0.0
          :simplification-probability 0.0
          :reproduction-probability 0.0
          :ultra-probability 1
          :ultra-alternation-rate 0.01
          :ultra-alignment-deviation 1
          :ultra-mutation-rate 0.05
          }))
