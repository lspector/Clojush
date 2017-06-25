;; odd.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.odd-with-uniform-silence-mutation
  (:use [clojush.pushgp.pushgp]
        [clojush.random]
        [clojush pushstate interpreter])
  (:require clojush.problems.demos.odd))

;;;;;;;;;;;;
;; The "odd" problem: take a positive integer input and push a Boolean indicating
;; whether or not the input is an odd number. There are many ways to compute this
;; and PushGP sometimes finds unusual methods.

(def argmap
  (merge clojush.problems.demos.odd/argmap
         {:epigenetic-markers [:close :silent]
          :genetic-operator-probabilities {:alternation 0.5
                                           :uniform-mutation 0.1
                                           [:alternation :uniform-mutation] 0.2
                                           :uniform-close-mutation 0.1
                                           :uniform-silence-mutation 0.1}
          :silent-instruction-probability 0.2
          :uniform-silence-mutation-rate 0.1
          }))
