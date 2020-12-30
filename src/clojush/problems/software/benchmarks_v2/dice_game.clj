;; dice_game.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.dice-game
  (:require [clojure.math.combinatorics :as combo])
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def dice-game-atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :exec :float])
    (list (tag-instruction-erc [:integer :boolean :exec :float] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1 'in2) ; inputs
   (list 0.0 1.0) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(defn dice-game-input
  []
  (let [die1 (inc (rand-int 100))
        die2 (inc (rand-int 100))]
      (cond
        (> die1 die2) [die2 die1]
        (< die1 die2) [die1 die2]
        :else [die1 (inc die2)])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def dice-game-data-domains
  [[(list [1 2]
          [99 100]
          [3 4]
          [4 6]
          [49 50]
          ) 5 0] ; Small and large cases
   [(fn [] (dice-game-input)) 195 2000] ; Random cases
   ])

;;Can make Dice Game test data like this:
;(test-and-train-data-from-domains dice-game-data-domains)

(defn dice-game-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
         (vector [in1 in2]
           (let [all-rolls (combo/cartesian-product (range 1 (inc in1)) (range 1 (inc in2)))]
             (float (/ (count (filter true? (map #(> (first %) (second %)) all-rolls)))
                       (count all-rolls))))))
       inputs))

(defn make-dice-game-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-dice-game-error-function
    ([individual]
      (the-actual-dice-game-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-dice-game-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1 input2] correct-output] (case data-cases
                                                     :train train-cases
                                                     :test test-cases
                                                     [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                        (push-item input2 :input)
                                                        (push-item input1 :input)))
                             result (stack-ref :float 0 final-state)]
                         (when print-outputs
                           (println (format "Correct output: %.3f | Program output: %.3f" correct-output result)))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is float error rounded to 3 decimal places
                         (round-to-n-decimal-places
                          (if (number? result)
                            (abs (- result correct-output)) ; distance from correct integer
                            1000000.0) ; penalty for no return value
                          3)
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-dice-game-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map dice-game-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def dice-game-train-and-test-cases
  (get-dice-game-train-and-test dice-game-data-domains))

(defn dice-game-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first dice-game-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second dice-game-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn dice-game-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Dice Game problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-dice-game-error-function-from-cases (first dice-game-train-and-test-cases)
                                                                   (second dice-game-train-and-test-cases))
   :atom-generators dice-game-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report dice-game-report
   :problem-specific-initial-report dice-game-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000.0
   })
