;; vector_average.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of floats with length in [1,50], with each float in [-1000,1000],
;; return the average of those floats. Results are rounded to 4 decimal places.
;;
;; input stack has 1 input vector of floats

(ns clojush.problems.software.vector-average
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def vector-average-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:vector_float :float :integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:vector_float :float :integer :exec])))


;; Define test cases
(defn vector-average-input
  "Makes a Vector Average input vector of length len."
  [len]
  (vec (repeatedly len
                   #(- (* (lrand) 2000.0) 1000.0))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def vector-average-data-domains
  [[(list [0.0] [100.0] [-100.0]) 3 0] ;; Length 1 vectors
   [(list [2.0 129.0]
          [0.12345 -4.678]
          [999.99 74.113]) 3 0] ;; Length 2 vectors
   [(fn [] (vector-average-input 50)) 4 50] ;; Length 50 vectors
   [(fn [] (vector-average-input (inc (lrand-int 50)))) 90 950] ;; Random length, random floats
   ])

;;Can make Vector Average test data like this:
;(test-and-train-data-from-domains vector-average-data-domains)

; Helper function for error function
(defn vector-average-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (/ (apply + %)
                   (count %)))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn vector-average-error-function
  "Returns the error function for the vector-average problem. Takes as
   input Vector Average data domains."
  [data-domains]
  (let [[train-cases test-cases] (map vector-average-test-cases
                                      (test-and-train-data-from-domains data-domains))]
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-vector-average-error-function
      ([program]
        (the-actual-vector-average-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-vector-average-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (doall
                       (for [[input1 correct-output] (case data-cases
                                                                  :train train-cases
                                                                  :test test-cases
                                                                  [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input1 :input)))
                               result (top-item :float final-state)]
                           (when print-outputs
                             (println (format "Correct output: %19.14f | Program output: %19.14f" correct-output result)))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj result))
                           ; Error is float error rounded to 4 decimal places
                           (round-to-n-decimal-places
                             (if (number? result)
                              (abs (- result correct-output)) ; distance from correct integer
                              1000000.0) ; penalty for no return value
                             4)
                           )))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(defn vector-average-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Vector Average problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 1.0E-3)
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best-program :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (vector-average-error-function vector-average-data-domains)
   :atom-generators vector-average-atom-generators
   :max-points 400
   :max-points-in-initial-program 200
   :evalpush-limit 800
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
   :problem-specific-report vector-average-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 1.0E-3
   :max-error 1000000.0
   })
