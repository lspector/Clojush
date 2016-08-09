;; negative_to_zero.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers in [-1000,1000] with length <= 50, return the
;; vector where all negative integers have been replaced by 0.
;;
;; NOTE: This problem gets lots of solutions that don't generalize. We could add
;; another error that finds the integer error at each position in vector with
;; penalty for wrong size of vector, which might help with generalization (but might not).
;;
;; input stack has 1 input vector of integers

(ns clojush.problems.software.negative-to-zero
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def negative-to-zero-atom-generators
  (concat (list
            0
            []
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :vector_integer :exec])))


;; Define test cases
(defn negative-to-zero-input
  "Makes a Negative To Zero input vector of length len with probability prob of being negative."
  [len prob]
  (vec (repeatedly len
                   #(if (< (lrand) prob)
                      (- (inc (lrand-int 1000)))
                      (lrand-int 1001)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def negative-to-zero-data-domains
  [[(list []) 1 0] ;; Empty vector
   [(concat (list [-10] [-1] [0] [1] [10])
            (repeatedly 5 #(vector (- (lrand-int 2001) 1000)))) 10 0] ;; Length 1 vectors
   [(list [0 0]
          [0 1]
          [-1 0]
          [-90 -6]
          [-16 33]
          [412 111]) 6 0] ;; Length 2 vectors
   [(fn [] (negative-to-zero-input (inc (lrand-int 50)) 1.0)) 9 100] ;; Random length, all negative
   [(fn [] (negative-to-zero-input (inc (lrand-int 50)) 0.0)) 9 100] ;; Random length, all positive
   [(fn [] (negative-to-zero-input (inc (lrand-int 50)) (lrand))) 165 1800] ;; Random length, random prob of negative
   ])

;;Can make Negative To Zero test data like this:
;(test-and-train-data-from-domains negative-to-zero-data-domains)

; Helper function for error function
(defn negative-to-zero-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (vec (map #(if (< % 0) 0 %)
                           in))))
       inputs))

(defn make-negative-to-zero-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-negative-to-zero-error-function
    ([program]
      (the-actual-negative-to-zero-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-negative-to-zero-error-function program data-cases false))
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
                             result (top-item :vector_integer final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (when @global-print-behavioral-diversity
                           (swap! behavior conj result))
                         ; Error is Levenshtein distance of vectors
                         (if (vector? result)
                           (levenshtein-distance correct-output result)
                           5000) ; penalty for no return value
                         )))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-negative-to-zero-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map negative-to-zero-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def negative-to-zero-train-and-test-cases
  (get-negative-to-zero-train-and-test negative-to-zero-data-domains))

(defn negative-to-zero-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first negative-to-zero-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second negative-to-zero-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn negative-to-zero-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Negative To Zero problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-negative-to-zero-error-function-from-cases (first negative-to-zero-train-and-test-cases)
                                                            (second negative-to-zero-train-and-test-cases))
   :atom-generators negative-to-zero-atom-generators
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
   :problem-specific-report negative-to-zero-report
   :problem-specific-initial-report negative-to-zero-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
