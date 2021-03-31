;; luhn.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem inspired by: https://www.codewars.com/kata/5418a1dd6d8216e18a0012b2

(ns clojush.problems.psb2.luhn
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :exec :vector_integer]) ; stacks
    (list (tag-instruction-erc [:integer :boolean :exec :vector_integer] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list 0
         2
         9
         10 ; constants
         (fn [] (- (lrand-int 21) 10))) ; integer ERC [-10, 10]
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))


; A list of data domains for the problem. Each domain is a vector containing
; a "set" of inputs and two integers representing how many cases from the set
; should be used as training and testing cases respectively. Each "set" of
; inputs is either a list or a function that, when called, will create a
; random element of the set.
(def data-domains
  [[(list
     [0 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3]
     [9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
     [5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5]
     [4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4]
     [1 0 2 0 4 3 2 1 0 4 1 2 3 4 2 1]
     [0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
     [2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0]
     [0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0]
     [0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0]
     [0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
     [0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0]
     [0 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0]
     [0 0 9 0 0 0 0 0 0 0 0 0 0 0 0 0]
     [8 0 0 0 0 6 0 0 0 0 9 0 0 0 0 0]
     [0 0 2 0 0 0 4 0 0 0 0 0 1 0 0 0]
     [0 5 0 5 0 5 0 5 0 5 0 5 0 5 0 5]
     [9 9 8 7 6 6 7 8 9 9 8 7 6 5 5 6]
     [0 0 0 0 0 7 0 0 0 3 0 0 0 0 0 0]) 20 0] ; "Special" inputs covering some base cases
   [(fn [] (vec (repeatedly 16 #(rand-int 10)))) 180 2000]])


; Helper function to multiply digit by 2 and subtract 9 (if necessary)
(defn luhn-formula
  [num]
  (if (>= (* num 2) 10)
    (- (* num 2) 9)
    (* num 2)))

; Helper function for error function
(defn create-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (reduce + (map #(% %2) (cycle [luhn-formula identity]) in))))
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
                         (println (format "Correct output: %s | Program output: %s" correct-output result)))
                         ; Record the behavior
                       (swap! behavior conj result)
                         ; Error is integer difference
                       (if (number? result)
                         (abs (- result correct-output)) ; distance from correct integer
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
    (printf ";; -*- Luhn problem report - generation %s\n" generation) (flush)
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
   :max-error 1000000})