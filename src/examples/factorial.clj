;; factorial.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns examples.factorial
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

;;;;;;;;;;;;
;; Integer symbolic regression of factorial, using an input instruction and 
;; lots of other instructions, a fairly large population (5000), and trivial
;; geography. Hard but solvable.

(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(defn factorial 
  "Returns the factorial of n. Just used to set up fitness cases here, so
  efficiency isn't a concern."
  [n]
  (if (< n 2)
    1
    (* n (factorial (- n 1)))))

(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [input (range 1 6)]
                        (let [state (run-push program
                                      (push-item input :auxiliary
                                        (push-item input :integer
                                          (make-push-state))))
                              top-int (top-item :integer state)]
                          (if (number? top-int)
                            (abs (- top-int (factorial input)))
                            1000000000))))) ;; big penalty, since errors can be big
	 :atom-generators (concat (registered-for-type :integer)
                     (registered-for-type :exec)
                     (registered-for-type :boolean)
                     (list (fn [] (rand-int 100))
                       'in))
	 :max-points 100
	 :population-size 5000
	 :trivial-geography-radius 10)