;; sum_of_squares.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given integer 0 < n <= 100, return the sum of squaring each positive integer
;; between 1 and n inclusive.
;;
;; input stack has integer n

(ns clojush.problems.software.sum-of-squares
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def sum-of-squares-atom-generators
  (concat (list
            0
            1
            ;;; end constants
            (fn [] (- (lrand-int 201) 100)) ;Integer ERC [-100,100]
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def sum-of-squares-data-domains
  [[(range 1 6) 5 0] ; Small cases
   [(list 100) 1 0] ; Last case
   [(fn [] (+ 6 (lrand-int 94))) 44 0] ; Random cases [6,99]
   [(range 1 101) 0 100] ; Test all integers in [1,100]
   ])

;;Can make Sum Of Squares test data like this:
;(test-and-train-data-from-domains sum-of-squares-data-domains)

; Helper function for error function
(defn sum-of-squares-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply +' (map #(*' % %) (range (inc in))))))
       inputs))

(defn make-sum-of-squares-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-sum-of-squares-error-function
    ([program]
      (the-actual-sum-of-squares-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-sum-of-squares-error-function program data-cases false))
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
                           (println (format "Correct output: %6d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (when @global-print-behavioral-diversity
                           (swap! behavior conj result))
                         ; Error is integer distance
                         (if (number? result)
                           (abs (- result correct-output)) ;distance from correct integer
                           1000000000) ;penalty for no return value
                         )))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-sum-of-squares-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map sum-of-squares-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def sum-of-squares-train-and-test-cases
  (get-sum-of-squares-train-and-test sum-of-squares-data-domains))

(defn sum-of-squares-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first sum-of-squares-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second sum-of-squares-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn sum-of-squares-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Sum Of Squares problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-sum-of-squares-error-function-from-cases (first sum-of-squares-train-and-test-cases)
                                                                  (second sum-of-squares-train-and-test-cases))
   :atom-generators sum-of-squares-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 4000
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
   :problem-specific-report sum-of-squares-report
   :problem-specific-initial-report sum-of-squares-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
