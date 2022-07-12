;; dice_game.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem inspired by: https://projecteuler.net/problem=205

(ns clojush.problems.psb2.dice-game
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :exec :float]) ; stacks
    (list (tag-instruction-erc [:integer :boolean :exec :float] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1 'in2) ; inputs
   (list 0.0 
         1.0) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(defn dice-game-input
  "Returns a vector containing the two inputs for the Dice Game problem.
   Each number represents the sides of a die, from 1-1,000 sides"
  []
  (let [die1 (inc (rand-int 1000))
        die2 (inc (rand-int 1000))]
    [die1 die2]))

; A list of data domains for the problem. Each domain is a vector containing
; a "set" of inputs and two integers representing how many cases from the set
; should be used as training and testing cases respectively. Each "set" of
; inputs is either a list or a function that, when called, will create a
; random element of the set.
(def data-domains
  [[(list [1 2]
          [2 1]
          [999 1000]
          [1000 999]
          [1 1000]
          [1000 1]
          [3 4]
          [4 3]
          [4 6]
          [6 4]
          [49 50]
          [500 493]
          [1 1]
          [500 500]
          [1000 1000]) 15 0] ; Small and large cases that have n < m, n = m, and n > m
   [(fn [] (let [x (inc (rand-int 1000))]
             [x x])) 10 100] ; 10 cases with n = m
   [(fn [] (dice-game-input)) 175 1900] ; Random cases
   ])

(defn create-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [[n m]]
         (vector [n m]
                 ; Mathed this solution
                 (if (<= n m)
                   (float (/ (dec n) (* 2 m)))
                   (float (- 1
                             (/ (inc m)
                                (* 2 n)))))))
       inputs))

(defn make-dice-game-error-function-from-cases
  "Creates and returns the error function based on the train/test cases."
  [train-cases test-cases]
  (fn the-actual-dice-game-error-function
    ([individual]
     (the-actual-dice-game-error-function individual :train))
    ([individual data-cases] ; data-cases should be :train or :test
     (the-actual-dice-game-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (doall
                   (for [[[input1 input2] correct-output] (case data-cases
                                                            :train train-cases
                                                            :test test-cases
                                                            data-cases)]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input2 :input)
                                                      (push-item input1 :input)))
                           result (stack-ref :float 0 final-state)]
                       (when print-outputs
                         (let [result-str (if (float? result)
                                            (format "%.3f" result)
                                            (str result))]
                           (println (format "Correct output: %.3f | Program output: %s" correct-output result-str))))
                         ; Record the behavior
                       (swap! behavior conj result)
                         ; Error is float error rounded to 3 decimal places
                       (round-to-n-decimal-places
                        (if (number? result)
                          (abs (- result correct-output)) ; distance from correct float
                          1000000.0) ; penalty for no return value
                        3))))]
       (if (= data-cases :test)
         (assoc individual :test-errors errors)
         (assoc individual
                :behaviors (reverse @behavior)
                :errors errors))))))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map create-test-cases
                 (test-and-train-data-from-domains data-domains))))

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
    (printf ";; -*- Dice Game problem report - generation %s\n" generation) (flush)
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
  {:error-function (make-dice-game-error-function-from-cases (first train-and-test-cases)
                                                             (second train-and-test-cases))
   :training-cases (first train-and-test-cases)
   :atom-generators atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5}
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report custom-report
   :problem-specific-initial-report initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000.0})