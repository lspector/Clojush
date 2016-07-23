;; vectors_summed.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given two vectors of integers in [-1000,1000] of the same length <= 50,
;; return a vector of integers that sums the other two at each index.
;;
;; input stack has 2 input vectors of integers

(ns clojush.problems.software.vectors-summed
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def vectors-summed-atom-generators
  (concat (list
            []
            ;;; end constants
            (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC [-1000,1000]
            ;;; end ERCs
            (tag-instruction-erc [:integer :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :vector_integer :exec])))


;; Define test cases
(defn vectors-summed-input
  "Makes a pair of Vectors Summed input vectors of length len."
  [len]
  (vector (vec (repeatedly len
                           #(- (lrand-int 2001) 1000)))
          (vec (repeatedly len
                           #(- (lrand-int 2001) 1000)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def vectors-summed-data-domains
  [[(list [[] []]) 1 0] ;; Empty vectors
   [(concat (list [[0] [0]]
                  [[0] [10]]
                  [[3] [5]]
                  [[7] [-9]]
                  [[-432] [-987]])
            (repeatedly 5 #(vectors-summed-input 1))) 10 0] ;; Length 1 vectors
   [(list [[0 0] [0 0]]
          [[0 1] [-4 2]]
          [[-1 0] [-3 0]]
          [[-90 -6] [-323 49]]) 4 0] ;; Length 2 vectors
   [(fn [] (vectors-summed-input 50)) 10 100] ;; Length 50 vectors
   [(fn [] (vectors-summed-input (inc (lrand-int 50)))) 125 1400] ;; Random length vectors
   ])

;;Can make Vectors Summed test data like this:
;(test-and-train-data-from-domains vectors-summed-data-domains)

; Helper function for error function
(defn vectors-summed-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (vec (map + (first in) (second in)))))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn vectors-summed-error-function
  "Returns the error function for the vectors-summed problem. Takes as
   input Vectors Summed data domains."
  [data-domains]
  (let [[train-cases test-cases] (map vectors-summed-test-cases
                                      (test-and-train-data-from-domains data-domains))]
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-vectors-summed-error-function
      ([program]
        (the-actual-vectors-summed-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-vectors-summed-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (doall
                       (for [[[input1 input2] correct-output] (case data-cases
                                                                           :train train-cases
                                                                           :test test-cases
                                                                           [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input2 :input)
                                                       (push-item input1 :input)))
                               result (top-item :vector_integer final-state)]
                           (when print-outputs
                             (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj result))
                           ; Error is integer error at each position in the vectors, with additional penalties for incorrect size vector
                           (if (vector? result)
                             (+' (apply +' (map (fn [cor res]
                                                  (abs (- cor res)))
                                                correct-output
                                                result))
                                 (*' 10000 (abs (- (count correct-output) (count result))))) ; penalty of 10000 times difference in sizes of vectors
                             1000000000) ; penalty for no return value
                           )))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(defn vectors-summed-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Vectors Summed problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
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
  {:error-function (vectors-summed-error-function vectors-summed-data-domains)
   :atom-generators vectors-summed-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 1500
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
   :problem-specific-report vectors-summed-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
