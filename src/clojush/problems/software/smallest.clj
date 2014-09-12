;; smallest.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: Program Repair Benchmark Paper (add citation later)
;;
;; Given 4 integers, print the smallest of them.
;;
;; input stack has 4 integers

(ns clojush.problems.software.smallest
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def smallest-atom-generators
  (concat (list
            ;;; end constants
            (fn [] (- (lrand-int 201) 100))
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tagERCs
            'in1
            'in2
            'in3
            'in4
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :print])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def smallest-data-domains
  [[['(0 0 0 0) '(-44 -44 -7 -13) '(0 4 -99 -33) '(-22 -22 -22 -22) '(99 100 99 100)] 5 0] ;; Edge cases by hand
   [(fn [] (shuffle (conj (repeat 3 (- (lrand-int 201) 100))
                          (- (lrand-int 201) 100)))) 10 100] ;; Edge cases where 3 of 4 are the same
   [(fn [] (repeat 4 (- (lrand-int 201) 100))) 5 100] ;; Edge cases where all are the same
   [(fn [] (repeatedly 4 #(lrand-int 101))) 20 200] ;; Each input includes 4 integers in range [0,100]
   [(fn [] (repeatedly 4 #(- (lrand-int 201) 100))) 60 600] ;; Each input includes 4 integers in range [-100,100]
   ])

(defn test-and-train-data-from-domains
  "Takes a list of domains and creates a set of (random) train inputs and a set of test
   inputs based on the domains. Returns [train test]. A program should not
   be considered a solution unless it is perfect on both the train and test
   cases."
  [domains]
  (apply mapv concat (map (fn [[input-set n-train n-test]]
                            (if (fn? input-set)
                              (vector (repeatedly n-train input-set)
                                      (repeatedly n-test input-set))
                              (vector (if (>= n-train (count input-set))
                                        input-set
                                        (take n-train (shuffle input-set)))
                                      (if (>= n-test (count input-set))
                                        input-set
                                        (take n-test (shuffle input-set))))))
                          domains)))

;;Can make Smallest test data like this:
;(test-and-train-data-from-domains smallest-data-domains)

; Helper function for error function
(defn smallest-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2 input3] output]."
  [inputs]
  (map #(vector %
                (apply min %))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn smallest-error-function
  "Returns the error function for the Smallest problem. Takes as
   input Smallest data domains."
  [data-domains]
  (let [[train-cases test-cases] (map smallest-test-cases
                                      (test-and-train-data-from-domains data-domains))]
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-smallest-error-function
      ([program]
        (the-actual-smallest-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-smallest-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (doall
                       (for [[[input1 input2 input3 input4] out-int] (case data-cases
                                                                                  :train train-cases
                                                                                  :test test-cases
                                                                                  [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input4 :input)
                                                       (push-item input3 :input)
                                                       (push-item input2 :input)
                                                       (push-item input1 :input)
                                                       (push-item "" :output)))
                               printed-result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "Correct output: %-19s | Program output: %-19s" (str out-int) printed-result)))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj printed-result))
                           ; Each test case is either right or wrong
                           (if (= printed-result (str out-int))
                             0
                             1))))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(defn smallest-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Smallest problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %d" i error))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best-program :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (smallest-error-function smallest-data-domains)
   :atom-generators smallest-atom-generators
   :max-points 200
   :max-points-in-initial-program 100
   :evalpush-limit 200
   :population-size 1000
   :max-generations 200
   :parent-selection :lexicase
   :epigenetic-markers [:close]
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 5
   :uniform-mutation-rate 0.01
   :problem-specific-report smallest-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   ;:max-error 1
   })
