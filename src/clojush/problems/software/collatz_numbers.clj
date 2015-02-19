;; collatz_numbers.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given an integer in the range [1,10000], find the number of terms in the
;; Collatz sequence starting from that integer.
;;
;; input stack has integer n

(ns clojush.problems.software.collatz-numbers
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def collatz-numbers-atom-generators
  (concat (list
            0
            1
            ;;; end constants
            (fn [] (- (lrand-int 201) 100)) ;Integer ERC [-100,100]
            ;;; end ERCs
            (tag-instruction-erc [:integer :float :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :float :boolean :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def collatz-numbers-data-domains
  [[(concat (range 1 11) '(9999 10000)) 12 0] ; Small and large cases
   [(list 6171 6943 7963 9257) 4 0] ; Cases with the largest answers in range: ([6171 262] [6943 257] [7963 252] [9257 260])
   [(fn [] (+ 11 (lrand-int 9988))) 184 2000] ; Random cases [11,9998]
   ])

;;Can make Collatz Numbers test data like this:
;(test-and-train-data-from-domains collatz-numbers-data-domains)

; Helper function for error function
(defn tail-collatz-sequence
  "Given a starting integer n, gives the number of steps in the Collatz sequence
   starting at n."
  [n]
  (loop [n n
         len 1]
    (if (>= 1 n)
      len
      (if (even? n)
        (recur (/ n 2) (inc len))
        (recur (inc (*' 3 n)) (inc len))))))

(defn collatz-numbers-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (tail-collatz-sequence in)))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn collatz-numbers-error-function
  "Returns the error function for the Collatz Numbers problem. Takes as
   input Collatz Numbers data domains."
  [data-domains]
  (let [[train-cases test-cases] (map sort (map collatz-numbers-test-cases
                                                (test-and-train-data-from-domains data-domains)))]
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-collatz-numbers-error-function
      ([program]
        (the-actual-collatz-numbers-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-collatz-numbers-error-function program data-cases false))
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
                               result (stack-ref :integer 0 final-state)]
                           (when print-outputs
                             (println (format "Correct output: %3d | Program output: %s" correct-output (str result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj result))
                           ; Error is difference of integers
                           (if (number? result)
                             (abs (- result correct-output)) ;distance from correct integer
                             1000000) ;penalty for no return value
                           )))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(defn collatz-numbers-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Collatz Numbers problem report - generation %s\n" generation)(flush)
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
  {:error-function (collatz-numbers-error-function collatz-numbers-data-domains)
   :atom-generators collatz-numbers-atom-generators
   :max-points 600
   :max-points-in-initial-program 300
   :evalpush-limit 15000
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
   :problem-specific-report collatz-numbers-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })

;;;;;;;;;;
;; Below here is for testing a hand-written solution.

;(reset! global-evalpush-limit 5000)
;
;(reset! global-max-points 600)
;
;(defn test-program-on-training
;  [program print-outputs]
;  ((collatz-numbers-error-function collatz-numbers-data-domains) program :train print-outputs))
;
;; This program only works if evalpush-limit > 4000 (works at 5000)
;(def tom-program
;  '(
;     tag_exec_100
;     (
;       integer_dup
;       1 integer_gt
;       exec_if
;       (
;        float_inc integer_dup
;        2 integer_mod 0 integer_eq
;        exec_if ; condition for even/odd (first even, then odd)
;        ( ;even case
;          2 integer_div
;          )
;        ( ;odd case
;          3 integer_mult integer_inc
;          )
;        tagged_50
;        )
;       (
;         integer_fromfloat
;         )
;       )
;     in1 1.0
;     tagged_50
;     ))
;
;(test-program-on-training tom-program false)
;
;(run-push tom-program
;          (push-item 5 :input (make-push-state)))
