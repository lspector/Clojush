;; change.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; This is code for the problem of determining the minimum number
;; of U.S. coins that can be returned to make change for a given amount.
;;
;; Input and output are given as single integers using the integer stack.

(ns clojush.problems.integer-regression.change
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random]
        clojush.instructions.tag
        clojure.math.numeric-tower))

; Make atom generators
(def change-atom-generators
  (list
    (fn [] (lrand-nth '(0 1 4 5 9 10 24 25)))
    (fn [] (- (lrand-int 101) 50))
    'integer_add
    'integer_swap
    'integer_yank
    'integer_dup
    'integer_yankdup
    'integer_shove
    'integer_mult
    'integer_div
    'integer_max
    'integer_sub
    'integer_mod
    'integer_rot
    'integer_min
    'in1))

; Create the error function
(defn make-change
  "This is the function that we want evolution to find."
  [amount-in]
  (loop [amount amount-in
         number-coins 0]
    (cond
      (zero? amount) number-coins
      (> 0 amount) "Error: amount is less than 0"
      (<= 25 amount) (recur (- amount 25) (inc number-coins))
      (<= 10 amount) (recur (- amount 10) (inc number-coins))
      (<= 5 amount) (recur (- amount 5) (inc number-coins))
      :else (+ number-coins amount))))

(defn change-test-cases
  "Creates the test cases for making change of amounts from 0 to number-test-cases."
  [number-test-cases]
  (map (fn [x] (vector x (make-change x)))
       (range number-test-cases)))

(defn change-error-function
  "Returns the error function for the change problem. Takes as input number
   of test cases to use."
  [number-test-cases]
  (fn [program]
    (doall
      (for [[input output] (change-test-cases number-test-cases)]
        (let [final-state (run-push program
                                    (push-item input
                                               :input
                                               (push-item input
                                                          :integer
                                                          (make-push-state))))
              result-output (top-item :integer final-state)]
          ; The error is the integer difference between the desired output
          ; and the result output.
          (if (number? result-output)
            (abs (- result-output output))
            1000))))))

; Define the argmap
(def argmap
  {:error-function (change-error-function 150)
   :atom-generators change-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 500
   :evalpush-limit 800
   :population-size 2000
   :max-generations 1000
   :epigenetic-markers []
   :genetic-operator-probabilities {[:alternation :uniform-mutation] 1.0} ;Basically do ULTRA
   :parent-selection :lexicase
   :report-simplifications 0
   :final-report-simplifications 1000
   })
