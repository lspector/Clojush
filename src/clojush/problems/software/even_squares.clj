;; even_squares.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given an integer 0 < n < 10000, print all of the positive even perfect
;; squares < n on separate lines.
;;
;; input stack has input integer n

(ns clojush.problems.software.even-squares
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower])
  (:require [clojure.string :as string]))

; Atom generators
(def even-squares-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :print])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def even-squares-data-domains
  [[(list 1 2 3 4 5 6 15 16 17 18 36 37 64 65) 14 0] ;; Small edge cases
   [(list 9600 9700 9999) 3 0] ;; Large edge cases
   [(fn [] (+ 20 (lrand-int 9980))) 83 1000] ;; Random cases
   ])

;;Can make Even Squares test data like this:
;(test-and-train-data-from-domains even-squares-data-domains)

; Helper function for error function
(defn even-squares-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (let [nums (rest (take-while #(< % in)
                                                 (map #(* 4 % %)
                                                      (range))))]
               (vector in
                       (vector (apply str (interpose \newline nums))
                               nums))))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn even-squares-error-function
  "Returns the error function for the Even Squares problem. Takes as
   input Even Squares data domains."
  [data-domains]
  (let [[train-cases test-cases] (map #(sort-by first %)
                                      (map even-squares-test-cases
                                           (test-and-train-data-from-domains data-domains)))]
    (when false ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-even-squares-error-function
      ([program]
        (the-actual-even-squares-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-even-squares-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (flatten
                       (doall
                         (for [[input1 [correct-output correct-integers]] (case data-cases
                                                                            :train train-cases
                                                                            :test test-cases
                                                                            [])]
                           (let [final-state (run-push program
                                                       (->> (make-push-state)
                                                         (push-item input1 :input)
                                                         (push-item "" :output)))
                                 result (stack-ref :output 0 final-state)]
                             (when print-outputs
                               (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                             ; Record the behavior
                             (when @global-print-behavioral-diversity
                               (swap! behavior conj result))
                             (let [correct-number-lines (count correct-integers)
                                   result-lines (if (= result "")
                                                  []
                                                  (string/split-lines result))
                                   int-parse-strings (filter #(re-matches #"-?\d+" %) result-lines)
                                   lines-with-integer-parseable-strings (count int-parse-strings)
                                   lines-without-integer-parseable-strings (- (count result-lines) lines-with-integer-parseable-strings)]
                               (vector
                                 ; Error 1: Levenshtein distance of printed strings
                                 (levenshtein-distance correct-output result)
                                 ; Error 2: Difference in number of lines with integer-parseable strings. Also, each line without an integer-parseable string contributes 1 error
                                 (+ (abs (- correct-number-lines lines-with-integer-parseable-strings))
                                    lines-without-integer-parseable-strings)
                                 ; Error 3: For each line in the result with a parseable integer, find the integer error compared to correct integer. Sum these.
                                 (let [correct-result-int-pairs (map vector
                                                                     correct-integers
                                                                     (concat (map (fn [int-str]
                                                                                    (try (Integer/parseInt int-str)
                                                                                      (catch Exception e :no-result)))
                                                                                  int-parse-strings)
                                                                             (repeat :no-result)))]
                                   (apply +' (map (fn [[cor-int res-int]]
                                                    (if (not (number? res-int))
                                                      100 ; penalty for not enough lines with parseable integers
                                                      (abs (- cor-int res-int))))
                                                  correct-result-int-pairs)))))))))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(defn even-squares-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Even Squares problem report - generation %s\n" generation)(flush)
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
  {:error-function (even-squares-error-function even-squares-data-domains)
   :atom-generators even-squares-atom-generators
   :max-points 400
   :max-points-in-initial-program 200
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
   :problem-specific-report even-squares-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })

;;;;;

;| Correct output: "4"
;| Program output: "4\n1514"
;
;| Correct output: "4"
;| Program output: ""
;
;| Correct output: "4\n16"
;| Program output: "4\n1716"
;
;| Correct output: "4\n16"
;| Program output: "4\n1817"
;
;| Correct output: "4\n16\n36\n64\n100\n144\n196\n256\n324\n400\n484\n576\n676\n784\n900\n1024\n1156\n1296\n1444\n1600\n1764\n1936\n2116\n2304\\
;n2500\n2704\n2916\n3136\n3364\n3600\n3844\n4096\n4356\n4624\n4900\n5184\n5476\n5776\n6084\n6400\n6724\n7056\n7396\n7744\n8100\n8464\n8836\n921\
;6"
;| Program output: "4\n16\n36\n64\n100\n144\n196\n256\n324\n400\n484\n576\n676\n784\n900\n1024\n1156\n1296\n1444\n1600\n1764\n1936\n2116\n2304\\
;n2500\n2704\n2916\n3136\n3364\n3600\n3844\n4096\n4356\n4624\n4900\n5184\n5476\n5776"
;
;| Correct output: "4\n16\n36\n64\n100\n144\n196\n256\n324\n400\n484\n576\n676\n784\n900\n1024\n1156\n1296\n1444\n1600\n1764\n1936\n2116\n2304\\
;n2500\n2704\n2916\n3136\n3364\n3600\n3844\n4096\n4356\n4624\n4900\n5184\n5476\n5776\n6084\n6400\n6724\n7056\n7396\n7744\n8100\n8464\n8836\n921\
;6\n9604"
;| Program output: "4\n16\n36\n64\n100\n144\n196\n256\n324\n400\n484\n576\n676\n784\n900\n1024\n1156\n1296\n1444\n1600\n1764\n1936\n2116\n2304\\
;n2500\n2704\n2916\n3136\n3364\n3600\n3844\n4096\n4356\n4624\n4900\n5184\n5476\n5776"

;;;;;;;;;;
;; Below here is for testing a hand-written solution.

;(reset! global-evalpush-limit 2000)
;
;(reset! global-max-points 400)
;
;(defn test-program-on-training
;  [program print-outputs]
;  ((even-squares-error-function even-squares-data-domains) program :train print-outputs))
;
;(def tom-program
;  '(
;     4 in1 integer_lt
;     exec_when
;     (
;       4 print_integer
;       4 integer_dup integer_dup integer_mult integer_dup in1 integer_lt
;       exec_while
;       (
;         print_newline print_integer 
;         integer_inc integer_inc
;         integer_dup integer_dup integer_mult integer_dup in1 integer_lt
;         )
;       )
;     ))
;
;(test-program-on-training tom-program false)
