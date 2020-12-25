;; bouncing_balls.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.bouncing-balls
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def bouncing-balls-atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :exec :float])
    (list (tag-instruction-erc [:exec :integer :boolean :float] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1 'in2 'in3) ; inputs
   (list 0.0 1.0 2.0) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

;; Define test cases
(defn bouncing-balls-input
  "Makes a Bouncing Balls input"
  []
  (let [start-height (inc (rand 99))
        bounce-height (inc (* (rand) (dec start-height)))]
        (vector start-height bounce-height (inc (rand-int 20)))))

;; A list of data domains for the bouncing-balls problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def bouncing-balls-data-domains
  [[(list [1.001 1.0 1] ; Smallest input
          [100.0 99.999 20] ; Largest input
          [100.0 1.0 20] ; Low bounce index
          [15.319 5.635 1]
          [2.176 1.787 1]
          [17.165 5.627 1]
          [60.567 37.053 1]
          [62.145 62.058 1]
          [36.311 33.399 1]
          [46.821 8.151 1] ; One bounce
          ) 10 0]
   [(fn [] (bouncing-balls-input)) 190 2000]])

;;Can make bouncing-balls test data like this:
;(test-and-train-data-from-domains bouncing-balls-data-domains)

; Helper function for error function
(defn bouncing-balls-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2 input3] output]."
  [inputs]
  (map (fn [[in1 in2 in3]]
          (vector [in1 in2 in3]
            (let [bounce-index (float (/ in2 in1))]
              (loop [start-height in1 bounce-height in2 bounces-left in3 distance 0]
                (if (= bounces-left 0) distance
                  (recur bounce-height (* bounce-height bounce-index) (dec bounces-left) (+ distance start-height bounce-height)))))))
       inputs))

(defn make-bouncing-balls-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-bouncing-balls-error-function
    ([individual]
      (the-actual-bouncing-balls-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-bouncing-balls-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1 input2 input3] correct-output] (case data-cases
                                                                    :train train-cases
                                                                    :test test-cases
                                                                    [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input3 :input)
                                                     (push-item input2 :input)
                                                     (push-item input1 :input)))
                             result (top-item :float final-state)]
                           (when print-outputs
                             (let [res-str (if (float? result)
                                             (format "%.3f" result)
                                             (str result))]
                               (println (format "Correct output: %.3f | Program output: %s" (float correct-output) res-str))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is float error rounded to 3 decimal places
                         (round-to-n-decimal-places
                          (if (number? result)
                            (abs (- result correct-output)) ; distance from correct integer
                            1000000.0) ; penalty for no return value
                          3))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-bouncing-balls-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map bouncing-balls-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def bouncing-balls-train-and-test-cases
  (get-bouncing-balls-train-and-test bouncing-balls-data-domains))

(defn bouncing-balls-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first bouncing-balls-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second bouncing-balls-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn bouncing-balls-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Bouncing Balls problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-bouncing-balls-error-function-from-cases (first bouncing-balls-train-and-test-cases)
                                                          (second bouncing-balls-train-and-test-cases))
   :atom-generators bouncing-balls-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :epigenetic-markers [:close]
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report bouncing-balls-report
   :problem-specific-initial-report bouncing-balls-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000.0
   })
