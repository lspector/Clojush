;; coin_sums.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem inspired by: https://projecteuler.net/problem=31

(ns clojush.problems.psb2.coin-sums
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]))

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

(define-registered
  output_integer3
  ^{:stack-types [:integer]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :output 2 state)))))

(define-registered
  output_integer4
  ^{:stack-types [:integer]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :output 3 state)))))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :exec]) ; stacks
    (list (tag-instruction-erc [:integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list 0
         1
         5
         10
         25) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

; A list of data domains for the problem. Each domain is a vector containing
; a "set" of inputs and two integers representing how many cases from the set
; should be used as training and testing cases respectively. Each "set" of
; inputs is either a list or a function that, when called, will create a
; random element of the set.
(def data-domains
  [[(range 1 31) 30 0] ; First 30 integers
   [(list 35
          41
          109 ; Interesting inputs
          10000 ; Max input
          )4 0]
   [(fn [] (inc (rand-int 10000))) 166 2000]])

(defn create-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input [output1 output2 output3 output4]]."
  [inputs]
  (map (fn [in]
         (vector in
                 (loop [money-left in pennies 0 nickles 0 dimes 0 quarters 0]
                   (cond
                     (= money-left 0) (vector pennies nickles dimes quarters)
                     (>= money-left 25) (recur (- money-left 25) pennies nickles dimes (inc quarters))
                     (>= money-left 10) (recur (- money-left 10) pennies nickles (inc dimes) quarters)
                     (>= money-left 5) (recur (- money-left 5) pennies (inc nickles) dimes quarters)
                     :else (recur (- money-left 1) (inc pennies) nickles dimes quarters)))))
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
           errors
           (flatten
            (doall
             (for [[input [correct-pennies correct-nickles correct-dimes correct-quarters]]
                   (case data-cases
                     :train train-cases
                     :test test-cases
                     data-cases)]
               (let [final-state (run-push (:program individual)
                                           (->> (make-push-state)
                                                (push-item :no-output :output)
                                                (push-item :no-output :output)
                                                (push-item :no-output :output)
                                                (push-item :no-output :output)
                                                (push-item input :input)))
                     result-pennies (stack-ref :output 0 final-state)
                     result-nickles (stack-ref :output 1 final-state)
                     result-dimes (stack-ref :output 2 final-state)
                     result-quarters (stack-ref :output 3 final-state)]
                 (when print-outputs
                   (println (format "Correct output: %s %s %s %s \nProgram output: %s %s %s %s\n"
                                    (str correct-pennies) (str correct-nickles) (str correct-dimes) (str correct-quarters)
                                    (str result-pennies) (str result-nickles) (str result-dimes) (str result-quarters))))
                           ; Record the behavior
                 (swap! behavior conj result-pennies result-nickles result-dimes result-quarters)
                           ; Error is integer difference for each integer
                 (vector
                  (if (number? result-pennies)
                    (abs (- result-pennies correct-pennies)) ; distance from correct integer
                    100000) ; penalty for no return value
                  (if (number? result-nickles)
                    (abs (- result-nickles correct-nickles)) ; distance from correct integer
                    100000) ; penalty for no return value
                  (if (number? result-dimes)
                    (abs (- result-dimes correct-dimes)) ; distance from correct integer
                    100000) ; penalty for no return value
                  (if (number? result-quarters)
                    (abs (- result-quarters correct-quarters)) ; distance from correct integer
                    100000) ; penalty for no return value
                  )))))] 
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
    (printf ";; -*- Coin Sums problem report - generation %s\n" generation) (flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 0.001)
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