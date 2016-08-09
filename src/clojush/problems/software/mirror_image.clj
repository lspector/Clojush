;; mirror_image.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given two lists of integers of the same length <= 50, return true if one
;; list is the reverse of the other, and false otherwise.
;;
;; input stack has 2 input vectors of integers

(ns clojush.problems.software.mirror-image
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def mirror-image-atom-generators
  (concat (list
            (fn [] (lrand-nth (list true false))) ;Boolean
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :vector_integer :exec])))


;; Define test cases
(defn mirror-image-input
  "Makes a Mirror Image input vector of length len."
  [len]
  (vec (repeatedly len
                   #(- (lrand-int 2001) 1000))))

(defn change-a-few-elements
  "Takes a vector and changes from 1 to 5 elements in the vector."
  ([in-vec]
    (change-a-few-elements in-vec (inc (lrand-int 5))))
  ([in-vec num-to-change]
    (if (>= 0 num-to-change)
      in-vec
      (change-a-few-elements (assoc in-vec (lrand-int (count in-vec)) (- (lrand-int 2001) 1000))
                             (dec num-to-change)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def mirror-image-data-domains
  [[(list [[] []]) 1 0] ;; Empty vectors
   [(list [[1] [1]]
          [[1] [0]]
          [[0] [1]]
          [[16] [-44]]
          [[-12] [-13]]) 5 0] ;; Length 1 vectors
   [(list [[1 2] [2 1]]
          [[1 1] [0 1]]
          [[7 0] [0 7]]
          [[5 8] [5 8]]
          [[34 12] [34 12]]
          [[456 456] [456 456]]
          [[-431 -680] [40 831]]) 7 0] ;; Length 2 vectors
   [(list [[1 2 1] [1 2 1]]
          [[1 2 3 4 5 4 3 2 1] [1 2 3 4 5 4 3 2 1]]
          [[45 99 0 12 44 7 7 44 12 0 99 45] [45 99 0 12 44 7 7 44 12 0 99 45]]
          [(vec (concat (reverse (range 25)) (range 25))) (vec (concat (reverse (range 25)) (range 25)))]) 4 0] ;; Equal Palindromes
   [(map #(vector [33 45 -941] (vec %))
         (permutations [33 45 -941])) 6 0] ;; Permutations of a 3 item vector
   [(fn [] (let [inA (mirror-image-input (inc (lrand-int 50)))]
             (vector inA (vec (reverse inA))))) 37 500] ;; true cases
   [(fn [] (let [inA (mirror-image-input (inc (lrand-int 50)))]
             (vector inA inA))) 10 100] ;; equal vector cases
   [(fn [] (let [inA (mirror-image-input (inc (lrand-int 50)))]
             (vector inA (change-a-few-elements (vec (reverse inA)))))) 20 200] ;; close calls cases (change a few elements at most)
   [(fn [] (let [inA (mirror-image-input (inc (lrand-int 50)))]
             (vector inA (mirror-image-input (count inA))))) 10 200] ;; totally random cases
   ])

;;Can make Mirror Image test data like this:
;(test-and-train-data-from-domains mirror-image-data-domains)

; Helper function for error function
(defn mirror-image-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (= (first %) (vec (reverse (second %)))))
       inputs))

(defn make-mirror-image-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-mirror-image-error-function
    ([program]
      (the-actual-mirror-image-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-mirror-image-error-function program data-cases false))
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
                             result (top-item :boolean final-state)]
                         (when print-outputs
                           (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (when @global-print-behavioral-diversity
                           (swap! behavior conj result))
                         ; Error is boolean error
                         (if (= result correct-output)
                           0
                           1))))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-mirror-image-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map mirror-image-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def mirror-image-train-and-test-cases
  (get-mirror-image-train-and-test mirror-image-data-domains))

(defn mirror-image-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first mirror-image-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second mirror-image-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn mirror-image-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Mirror Image problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-mirror-image-error-function-from-cases (first mirror-image-train-and-test-cases)
                                                                (second mirror-image-train-and-test-cases))
   :atom-generators mirror-image-atom-generators
   :max-points 1200
   :max-genome-size-in-initial-program 150
   :evalpush-limit 600
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
   :problem-specific-report mirror-image-report
   :problem-specific-initial-report mirror-image-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1
   })
