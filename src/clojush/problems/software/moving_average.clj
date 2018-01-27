;; moving_average.clj
;; Anil Saini, aks@cs.umass.edu
;;
;; Given vector of integers in [-1000,1000] of the length n<= 50 and an integer m,
;; return a float vector containing moving averages of the given vector with a window of size m (0<m<=n).
;;
;; Given a series of numbers and a fixed subset size, the first element of the moving average 
;; is obtained by taking the average of the initial fixed subset of the number series. 
;; Then the subset is modified by "shifting forward"; that is, excluding the first number 
;; of the series and including the next value in the subset.
;;
;; input stack has one input vectors of integers and an additional integer specifying the window size.

(ns clojush.problems.software.moving-average
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def moving-average-atom-generators
  (concat (list
            []
            ;;; end constants
            (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC [-1000,1000]
            ;;; end ERCs
            ;(tag-instruction-erc [:integer :float :vector_float :exec] 1000)
            ;(tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :float :vector_float :exec])))


;; Define test cases
(defn moving-average-input
  "Makes a a vector of given vector and a random interger less than len."
  [len]
  (vector (vec (repeatedly len
                           #(- (* (lrand) 2000.0) 1000)))
          (inc (lrand-int len))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def moving-average-data-domains
  [[(list [[] 1]) 1 0] ;; Empty vectors
   [(concat (list [[0.0] 1]
                  [[0.4] 1]
                  [[3.12] 1]
                  [[7.23] 1]
                  [[-432.12] 1])
            (repeatedly 5 #(moving-average-input 1))) 10 0] ;; Length 1 vectors
   [(list [[0.0 0.3] 1]
          [[0.4 1.5] 2]
          [[-1.0 0.0] 1]
          [[-90.0 -6.0] 2]) 4 0] ;; Length 2 vectors
   [(fn [] (moving-average-input 50)) 10 100] ;; Length 50 vectors
   [(fn [] (moving-average-input (inc (lrand-int 50)))) 125 1400] ;; Random length vectors
   ])

;;Can make Vectors Summed test data like this:
;(test-and-train-data-from-domains moving-average-data-domains)

; Helper function for error function
(defn moving-average-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
         (let [lst (first in)
               window (second in)]
         (vec (map (fn [lst] (float (/ (reduce + lst) (count lst))))
                   (partition window 1 lst))))))
       inputs))

(defn make-moving-average-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-moving-average-error-function
    ([individual]
      (the-actual-moving-average-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-moving-average-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[lst win] correct-output] (case data-cases
                                                        :train train-cases
                                                        :test test-cases
                                                        [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item win :input)
                                                     (push-item lst :input)))
                             result (top-item :vector_float final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer error at each position in the vectors, with additional penalties for incorrect size vector
                         (if (vector? result)
                           (+' (apply +' (map (fn [cor res]
                                                (abs (- cor res)))
                                              correct-output
                                              result))
                               (*' 10000 (abs (- (count correct-output) (count result))))) ; penalty of 10000 times difference in sizes of vectors
                           1000000000) ; penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-moving-average-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map moving-average-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def moving-average-train-and-test-cases
  (get-moving-average-train-and-test moving-average-data-domains))

(defn moving-average-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first moving-average-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second moving-average-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn moving-average-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
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
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-moving-average-error-function-from-cases (first moving-average-train-and-test-cases)
                                                                  (second moving-average-train-and-test-cases))
   :atom-generators moving-average-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 1500
   :population-size 1000
   :max-generations 500
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation  0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report moving-average-report
   :problem-specific-initial-report moving-average-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
