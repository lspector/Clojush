;; fuel_cost.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem inspired by: https://adventofcode.com/2019/day/1

(ns clojush.problems.software.benchmarks-v2.fuel-cost
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag)
  (:require [clojure.math.numeric-tower :as nt]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :vector_integer :boolean :exec])
    (list (tag-instruction-erc [:integer :vector_integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list 0
         1
         2
         3
         (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC
         ) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(defn fuel-cost-input
  "Creates a vector of length len of integers in the range [6, 10000]"
  [len]
  (vec (repeatedly len #(+ 6 (lrand-int 9995)))))

;; A list of data domains. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(list [6] [7] [8] [9] [10] [11] [12] [13] [14] [15] [16] [17]
          [9998] [9999] [10000]) 15 0] ; length-1 vectors
   [(list [6 6]
          [9 14]
          [9 15]
          [14 9]
          [15 9]
          [32 32]
          [33 33]
          [10000 9]
          [9 10000]
          [10000 10000]) 10 0] ; length-2 vectors
   [(list (vec (repeat 20 6))
          (vec (repeat 20 7))
          (vec (repeat 20 8))
          (vec (repeat 20 9))
          (vec (repeat 20 10))
          (vec (repeat 20 11))
          (vec (repeat 20 12))
          (vec (repeat 20 13))
          (vec (repeat 20 9998))
          (vec (repeat 20 9999))
          (vec (repeat 20 10000))
          (vec (repeat 15 9))) 12 0] ; long vectors of edge-case integers
   [(fn [] (fuel-cost-input (inc (lrand-int 5)))) 75 1000] ;random short vectors, length [1, 5]
   [(fn [] (fuel-cost-input (+ 6 (lrand-int 15)))) 88 1000] ; random longer vectors, length [6, 20]
   ])

;;Can make test data like this:
;(test-and-train-data-from-domains data-domains)

; Helper function for error function
(defn test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply + (map #(- (quot % 3)
                                   2)
                               in))))
       inputs))

(defn make-error-function-from-cases
  "Creates and returns the error function based on the train/test cases."
  [train-cases test-cases]
  (fn the-actual-error-function
    ([individual]
      (the-actual-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input1 correct-output] (case data-cases
                                                              :train train-cases
                                                              :test test-cases
                                                              [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                        (push-item input1 :input)))
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %9d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer difference
                         (if (number? result)
                           (nt/abs (- result correct-output)) ; distance from correct integer
                           max-number-magnitude) ; penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def train-and-test-cases
  (get-train-and-test data-domains))

(defn initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn custom-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Find Pair problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-error-function-from-cases (first train-and-test-cases)
                                                   (second train-and-test-cases))
   :atom-generators atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1.0}
   :uniform-addition-and-deletion-rate 0.09
   :problem-specific-report custom-report
   :problem-specific-initial-report initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error max-number-magnitude})
