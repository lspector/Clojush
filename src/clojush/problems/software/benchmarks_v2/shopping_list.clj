;; shopping_list.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.shopping-list
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def shopping-list-atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :exec :float :vector_float])
    (list (tag-instruction-erc [:integer :boolean :exec :float :vector_float] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1 'in2) ; inputs
   (list 0.0
         100.0
         (fn [] (* (lrand) 100)) ; float ERC [0, 100]
         ) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(defn shopping-list-input
  "Makes a Shopping List input vector of length len."
  [len]
  (vector (vec (repeatedly len #(round-to-n-decimal-places (* (rand) 50) 2)))
          (vec (repeatedly len #(round-to-n-decimal-places (* (rand) 100) 2)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def shopping-list-data-domains
  [[(list [[50.00] [100.00]]
          [[50.00] [10.00]]
          [[20.00 20.00] [100.00 50.00]]
          [[20.00 20.00] [20.00 0.00]]
          [[10.00 20.00 30.00] [5.00 10.00 95.00]]
          [[43.14 18.23 5.33 1.35 39.68] [100.00 100.00 100.00 100.00 100.00]]
          [[5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73 5.73]
           [59.19 91.24 25.93 16.18 24.65 61.96 67.91 43.87 36.23 34.30 96.27 69.25 73.78 0.52 8.91 39.18 79.67 64.22 14.15 52.44]]
          [[25.43 43.22 23.42 42.09 25.70] [0.00 0.00 0.00 0.00 0.00]]
          [[0.01 0.01 0.01 0.01 0.01 0.01 0.01] [85.77 43.99 22.78 34.14 34.12 8.54 11.03]]
          [[9.99 9.99 9.99 9.99 9.99 9.99 9.99 9.99 9.99 9.99] [33.65 33.65 33.65 33.65 33.65 33.65 33.65 33.65 33.65 33.65]])
    10 0]
   [(fn [] (shopping-list-input (inc (lrand-int 20)))) 190 2000]
   ])

;;Can make Shopping List test data like this:
;(test-and-train-data-from-domains shopping-list-data-domains)

; Some code from here: https://stackoverflow.com/questions/36105612/map-a-function-on-every-two-elements-of-a-list
(defn shopping-list-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
         (vector [in1 in2]
           (let [discount (map #(float (/ (- 100 %) 100)) in2)]
             (round-to-n-decimal-places (reduce + (vec (map #(* % %2) in1 discount))) 2))))
       inputs))

(defn make-shopping-list-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-shopping-list-error-function
    ([individual]
      (the-actual-shopping-list-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-shopping-list-error-function individual data-cases false))
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
                             result (top-item :float final-state)]
                             (when print-outputs
                               (let [res-str (if (float? result)
                                               (format "%.2f" result)
                                               (str result))]
                                 (println (format "Correct output: %.2f | Program output: %s" (float correct-output) res-str))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is float error rounded to 2 decimal places
                         (round-to-n-decimal-places
                          (if (number? result)
                            (abs (- result correct-output)) ; distance from correct integer
                            1000000.0) ; penalty for no return value
                          2)
                           )))]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual
                 :behaviors (reverse @behavior)
                 :errors errors))))))

(defn get-shopping-list-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map shopping-list-test-cases
      (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def shopping-list-train-and-test-cases
  (get-shopping-list-train-and-test shopping-list-data-domains))

(defn shopping-list-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first shopping-list-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second shopping-list-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn shopping-list-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Shopping List problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-shopping-list-error-function-from-cases (first shopping-list-train-and-test-cases)
                                                                 (second shopping-list-train-and-test-cases))
   :training-cases (first shopping-list-train-and-test-cases)
   :atom-generators shopping-list-atom-generators
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
   :problem-specific-report shopping-list-report
   :problem-specific-initial-report shopping-list-initial-report
   :final-report-simplifications 5000
   :max-error 1000000.0})
