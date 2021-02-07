;; find_pair.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem inspired by: https://adventofcode.com/2020/day/1

(ns clojush.problems.software.benchmarks-v2.find-pair
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag)
  (:require [clojure.math.numeric-tower :as nt]))

(define-registered
  output_integer1
  ^{:stack-types [:integer]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :output 0 state)))))

(define-registered
  output_integer2
  ^{:stack-types [:integer]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :output 1 state)))))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :vector_integer :boolean :exec])
    (list (tag-instruction-erc [:integer :vector_integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1 'in2) ; inputs
   (list 0
         1
         2
         -1
         (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC
         ) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(defn all-pairs-sum-to-target
  "Finds all pairs of numbers in list that sum to target and returns them"
  [nums target]
  (for [x nums
        y nums
        :when (= target (+ x y))]
    [x y]))

;; Define test cases
(defn find-pair-input
  "Makes a random Find Pair input, returning a pair of [vector, target].
   Needs to make sure that the two chosen numbers in the vector that sum
   to target are the only two. Otherwise, problem could have multiple right
   answers. Those numbers must also be different."
  [vector-length]
  (let [numbers (vec (distinct (repeatedly vector-length
                                           #(- (lrand-int 20001) 10000))))
        num1 (rand-nth numbers)
        num2 (rand-nth numbers)
        target (+ num1 num2)]
    (if (or (= num1 num2)
            (< (count numbers) 2)
            (> (count (all-pairs-sum-to-target numbers target)) 2))
      (recur vector-length) ; if bad vector, just make another
      [numbers target])))

;; A list of data domains. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(list [[5 7] 12]
          [[2500 6352] 8852]
          [[-14 5] -9]
          [[40 -19] 21]
          [[-4 4] 0]
          [[-5 -20] -25]
          [[-7831 -3001] -10832]
          [[10000 -10000] 0]
          [[8310 -8320] -10]
          [[0 1234] 1234]) 10 0] ; length-2 vectors
   [(list [[1 3 5] 4]
          [[1 3 5] 6]
          [[1 3 5] 8]
          [[-1 0 1] 0]
          [[-1 0 1] 1]
          [[-1 0 1] -1]
          [[14 -35 78] -21]
          [[14 -35 78] 92]
          [[14 -35 78] 43]
          [[9492 -3791 -7317] 5701]
          [[9492 -3791 -7317] 2175]
          [[9492 -3791 -7317] -11108]
          [[237 410 -777] 647]
          [[237 410 -777] -540]
          [[237 410 -777] -367]) 15 0] ; length-3 vectors
   [(fn [] (find-pair-input 4)) 25 0] ;random length-4 vectors
   [(fn [] (find-pair-input 5)) 25 0] ;random length-5 vectors
   [(fn [] (find-pair-input (+ 2 (lrand-int 19)))) 125 2000] ; random vectors, length [2, 20]
   ])

;;Can make test data like this:
;(test-and-train-data-from-domains data-domains)

; Helper function for error function
(defn test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [[in1 in2]]
          (vector [in1 in2]
                  (first (all-pairs-sum-to-target in1 in2))))
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
            errors (flatten
                    (doall
                     (for [[[input1 input2] [correct-output1 correct-output2]] (case data-cases
                                                                                 :train train-cases
                                                                                 :test test-cases
                                                                                 data-cases)]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                        (push-item :no-output :output)
                                                        (push-item :no-output :output)
                                                        (push-item input2 :input)
                                                        (push-item input1 :input)))
                             result1 (stack-ref :output 0 final-state)
                             result2 (stack-ref :output 1 final-state)]
                         (when print-outputs
                           (println (format "Correct output: %s %s | Program output: %s %s"
                                            (str correct-output1) (str correct-output2)
                                            (str result1) (str result2))))
                         ; Record the behavior
                         (swap! behavior conj result1 result2)
                         ; Error is integer distance
                         (vector
                          (if (number? result1)
                            (nt/abs (- result1 correct-output1)) ;distance from correct integer
                            1000000) ;penalty for no return value
                          (if (number? result2)
                            (nt/abs (- result2 correct-output2)) ;distance from correct integer
                            1000000) ;penalty for no return value
                          )))))]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual
                 :behaviors (reverse @behavior)
                 :errors errors))))))

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
   :training-cases (first train-and-test-cases)
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
   :max-error 1000000})
