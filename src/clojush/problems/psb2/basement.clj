;; basement.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem inspired by: https://adventofcode.com/2015/day/1

(ns clojush.problems.psb2.basement
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag)
  (:require [clojure.math.numeric-tower :as nt]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :vector_integer :boolean :exec]) ; stacks
    (list (tag-instruction-erc [:integer :vector_integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list []
         -1
         0
         1 ; constants
         (fn [] (- (lrand-int 2001) 1000))) ; integer ERC
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(defn solve-basement
  "Given a vector of integers, find the index of the first one at which
   all elements before and up to it summed are negative. If no such index
   exists, returns nil"
  [numbers]
  (loop [index 0
         sum 0]
    (cond
      (>= index (count numbers)) nil           ; reached end of vector
      (< (+ sum (nth numbers index)) 0) index  ; answer index
      :else (recur (inc index)
                   (+ sum (nth numbers index))))))

(defn basement-input-skewer
  "Makes an attempt at a basement problem input.
   Closer to the beginning of the vector has more chance of being positive,
   and closer to the end of the vector has more chance of being negative."
  [len]
  (loop [index 0
         vec-nums []]
    (if (>= (count vec-nums) len)
      vec-nums
      (let [probability-of-negative (/ (inc index) (inc len))
            ; this will skew the probability of choosing a negative number
            ; based on the index, with lower indices having lower chance
            number (if (< (lrand) probability-of-negative)
                     (- (lrand-int 101))
                     (lrand-int 101))]
        (recur (inc index)
               (conj vec-nums number))))))

(defn basement-input
  "Creates a vector of integers of length len for the basement problem.
   There must be an index in the vector at which point the sum of the
   elements from index 0 to that index is negative.
   The answer-index is chosen uniformly between 0 and (dec len), to ensure
   a uniform distribution of answer indices."
  [len]
  (let [answer-index (lrand-int len)]
    (loop []
      (let [attempt (basement-input-skewer len)]
        (if (= answer-index (solve-basement attempt))
          attempt
          (recur))))))

; A list of data domains. Each domain is a vector containing
; a "set" of inputs and two integers representing how many cases from the set
; should be used as training and testing cases respectively. Each "set" of
; inputs is either a list or a function that, when called, will create a
; random element of the set.
(def data-domains
  [[(list [-1] [-100]) 2 0] ; Length-1 fixed vectors
   [(list [-1 100]
          [-95 100]
          [-30 5]
          [-50 50]
          [0 -1]
          [1 -2]
          [3 -97]
          [99 -100]) 8 0] ; Length-2 fixed vectors
   [(list [1 -1 -1]
          [1 -2 -1]
          [0 -1 -1]
          [0 0 -1]
          [2 -2 -1]
          [5 -6 -5]
          [5 -5 -5]
          [100 -100 -100]
          [-100 -100 -100]
          [-1 100 99]) 10 0] ; Length-3 fixed vectors, specifically to exhibit the dividing line between 0 and -1
   [(list (vec (repeat 20 -10))
          [-1 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100]
          [5 -10 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5]
          [5  5  5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 -99]
          [1 2 3 4 5 6 7 8 9 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 10]
          [93 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5 -5]
          [99 -10 -9 -12 -10 -5 -12 -3 -10 -9 -9 -10 -12 -11 -12 -9 -10 -12 -11 -10]
          [1 1 1 1 2 2 2 2 -1 -1 -1 -2 -3 -4 -5 -1 1 2 3 4]
          [0 -1 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 -1]
          [0 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 -1]
          [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1]
          [50 -30 0 0 0 0 0 0 0 0 -30 0 0 0 0 0 0 0 0 -30]
          [50 0 0 0 0 0 0 0 0 0 -30 0 0 0 0 0 0 0 0 -30]
          [50 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -60]
          [20 -30 0 0 0 0 0 0 0 0 -30 0 0 0 0 0 0 0 0 -30]) 15 0] ; Length-20 fixed strings
   [(fn [] (basement-input (inc (lrand-int 20)))) 165 2000] ; Random vectors, length [1, 20]
   ])

; Helper function for error function
(defn create-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (solve-basement in)))
       inputs))

(defn make-error-function-from-cases
  "Creates and returns the error function based on the train/test cases."
  [train-cases test-cases]
  (fn the-actual-error-function
    ([individual]
     (the-actual-error-function individual :train))
    ([individual data-cases] ; data-cases should be :train or :test
     (the-actual-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (doall
                   (for [[input1 correct-output] (case data-cases
                                                   :train train-cases
                                                   :test test-cases
                                                   data-cases)]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)))
                           result (top-item :integer final-state)]
                       (when print-outputs
                         (println (format "Correct output: %3d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                       (swap! behavior conj result)
                         ; Error is integer difference
                       (if (number? result)
                         (nt/abs (- result correct-output)) ; distance from correct integer
                         1000000) ; penalty for no return value
                       )))]
       (if (= data-cases :test)
         (assoc individual :test-errors errors)
         (assoc individual
                :behaviors (reverse @behavior)
                :errors errors))))))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map create-test-cases
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
    (printf ";; -*- Find Pair problem report - generation %s\n" generation) (flush)
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
    )) ; To do validation, could have this function return an altered best individual
       ; with total-error > 0 if it had error of zero on train but not on validation
       ; set. Would need a third category of data cases, or a defined split of training cases.

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