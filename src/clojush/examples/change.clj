;; change.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; This is code for the problem of determining the minimum number
;; of U.S. coins that can be returned to make change for a given amount.
;;
;; Input and output are given as single integers using the integer stack.

(ns clojush.examples.change
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random]
        clojure.math.numeric-tower))

; Make atom generators
(define-registered 
  in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(def change-atom-generators
  (list
    (fn [] (lrand-nth '(0 1 4 5 9 10 24 25)))
    (fn [] (- (lrand-int 101) 50))
    (tag-instruction-erc [:exec :integer] 1000)
    (tagged-instruction-erc 1000)
    'integer_add
    'integer_eq
    'integer_swap
    'integer_yank
    'integer_dup
    'integer_yankdup
    'integer_lt
    'integer_shove
    'integer_mult
    'integer_stackdepth
    'integer_div
    'integer_gt
    'integer_max
    'integer_fromboolean
    'integer_sub
    'integer_mod
    'integer_rot
    'integer_min
    'exec_y
    'exec_pop
    'exec_eq
    'exec_stackdepth
    'exec_rot
    'exec_when
    'exec_do*times
    'exec_do*count
    'exec_s
    'exec_do*range
    'exec_if
    'exec_k
    'exec_yank
    'exec_yankdup
    'exec_swap
    'exec_dup
    'exec_shove
    'boolean_swap
    ;'boolean_eq
    ;'boolean_rot
    'boolean_and
    'boolean_not
    'boolean_or
    'boolean_frominteger
    'boolean_stackdepth
    'boolean_dup
    'in))

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
                                               :auxiliary 
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
   :max-points 500
   :max-points-in-initial-program 500
   :evalpush-limit 1000
   :population-size 1000
   :max-generations 500
   :mutation-probability 0
   :mutation-max-points 50
   :crossover-probability 0
   :tournament-size 7
   :report-simplifications 0
   :final-report-simplifications 1000
   :use-lexicase-selection true
   :ultra-probability 1.0
   })