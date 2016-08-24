;; compare_string_lengths.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given three strings in1, in2, and in3, return true if
;; lenth(in1) < length(in2) < length(in3), and false otherwise.
;;
;; input stack has 3 input strings

(ns clojush.problems.software.compare-string-lengths
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def csl-atom-generators
  (concat (list
            (fn [] (lrand-nth (list true false))) ;Boolean ERC
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :string :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :exec])))


;; Define test cases
(defn csl-input
  "Makes a Compare String Lengths input string of length len."
  [len]
  (apply str
         (repeatedly len
                     #(lrand-nth (concat [\newline \tab]
                                         (map char (range 32 127)))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def csl-data-domains
  [[(list ["" "" ""]) 1 0] ;; All empty strings
   [(permutations ["" "a" "bc"]) 6 0] ;; Permutations of three small strings
   [(apply concat (repeatedly 2 #(permutations ["" "" (csl-input (inc (lrand-int 49)))]))) 6 0] ;; Cases with 2 empties and a non-empty
   [(apply concat (repeatedly 3 #(permutations (conj (repeat 2 (csl-input (inc (lrand-int 49)))) (csl-input (inc (lrand-int 49))))))) 9 0] ;; Cases with 2 strings repeated
   [(fn [] (repeat 3 (csl-input (lrand-int 50)))) 3 100] ;; Cases where all are the same
   [(fn [] (sort-by count (repeatedly 3 #(csl-input (lrand-int 50))))) 25 200] ;; Cases forced to be in order (as long as two aren't same size randomly, will be true)
   [(fn [] (repeatedly 3 #(csl-input (lrand-int 50)))) 50 700] ;; Cases in random order
   ])

;;Can make Compare String Lengths test data like this:
;(test-and-train-data-from-domains csl-data-domains)

; Helper function for error function
(defn csl-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (apply < (map count %)))
       inputs))

(defn make-compare-string-lengths-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-csl-error-function
    ([program]
      (the-actual-csl-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-csl-error-function program data-cases false))
    ([program data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1 input2 input3] correct-output] (case data-cases
                                                                     :train train-cases
                                                                     :test test-cases
                                                                     [])]
                       (let [final-state (run-push program
                                                   (->> (make-push-state)
                                                     (push-item input3 :input)
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
  
(defn get-compare-string-lengths-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map csl-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def compare-string-lengths-train-and-test-cases
  (get-compare-string-lengths-train-and-test csl-data-domains))

(defn compare-string-lengths-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first compare-string-lengths-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second compare-string-lengths-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn csl-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Compare String Lengths problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-compare-string-lengths-error-function-from-cases (first compare-string-lengths-train-and-test-cases)
                                                                          (second compare-string-lengths-train-and-test-cases))
   :atom-generators csl-atom-generators
   :max-points 800
   :max-genome-size-in-initial-program 200
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
   :problem-specific-report csl-report
   :problem-specific-initial-report compare-string-lengths-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1
   })
