;; vector_distance.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.vector-distance
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def vector-distance-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec :float :vector_float] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :float :vector_float])))

(defn vector-distance-input
  "Makes a Vector Distance input vector of length len."
  [len]
  (vector (vec (repeatedly len #(- (* (rand) 200) 100))) (vec (repeatedly len #(- (* (rand) 200) 100)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def vector-distance-data-domains
  [[(list [[-100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0 -100.0]
           [100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0]]
          [[5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32]
           [5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32 5.32]]
          [[42.91283] [-22.1340]]
          [[1.5 2.87 3.3324 4.654 5.123 6.867 7.5324 8.534 9.4132 10.43]
           [-1.534 -2.543 -3.423 -4.13427 -5.714 -6.713 -7.4328 -8.43 -9.73 -10.752]]
          [[0.4378 0.634 0.1234 0.764 0.243] [-0.254 -0.1223 -0.19582 -0.8971 -0.8743]]) 5 0]
   [(fn [] (vector-distance-input 1)) 5 50] ; Size 1 inputs
   [(fn [] (vector-distance-input 2)) 30 300] ; Size 2 inputs
   [(fn [] (vector-distance-input 3)) 30 300] ; Size 3 inputs
   [(fn [] (vector-distance-input 4)) 20 200] ; Size 4 inputs
   [(fn [] (vector-distance-input 5)) 10 100] ; Size 5 inputs
   [(fn [] (vector-distance-input (+ (lrand-int 15) 6))) 100 1050]
   ])

;;Can make Vector Distance test data like this:
;(test-and-train-data-from-domains vector-distance-data-domains)

; Helper function for error function
(defn euclidean-distance
  "Calculates the Euclidean squared distance between two points x and y."
  [x y]
  (expt (- x y) 2))

(defn vector-distance-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
         (vector [in1 in2]
           (sqrt (reduce + (map euclidean-distance in1 in2)))))
       inputs))

(defn make-vector-distance-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-vector-distance-error-function
    ([individual]
      (the-actual-vector-distance-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-vector-distance-error-function individual data-cases false))
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
                          3)
                           )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-vector-distance-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map vector-distance-test-cases
      (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def vector-distance-train-and-test-cases
  (get-vector-distance-train-and-test vector-distance-data-domains))

(defn vector-distance-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first vector-distance-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second vector-distance-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn vector-distance-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Vector Distance problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-vector-distance-error-function-from-cases (first vector-distance-train-and-test-cases)
                                                                   (second vector-distance-train-and-test-cases))
   :atom-generators vector-distance-atom-generators
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
   :problem-specific-report vector-distance-report
   :problem-specific-initial-report vector-distance-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000.0
   })
