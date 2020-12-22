;; gcd.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.gcd
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def gcd-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:boolean :integer :exec])))


;; Define test cases
(defn gcd-input
  "Makes a GCD input vector."
  []
  (vector (inc (rand-int 1000000)) (inc (rand-int 1000000))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def gcd-data-domains
  [[(list [1 1]
          [4 400000]
          [54 24]
          [4200 3528]
          [820000 63550]
          [123456 654321]) 6 0]
   [(fn [] (gcd-input)) 194 2000] ;; Random length, random floats
   ])

;;Can make GCD test data like this:
;(test-and-train-data-from-domains gcd-data-domains)

; Helper function for error function
; Code from math.numeric-tower
(defn gcd-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
         (vector [in1 in2]
           (loop [a in1 b in2]
             (if (zero? b) a
             (recur b (mod a b))))))
       inputs))

(defn make-gcd-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-gcd-error-function
    ([individual]
     (the-actual-gcd-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-gcd-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (doall
                   (for [[[input1 input2] correct-output] (case data-cases
                                                           :train train-cases
                                                           :test test-cases
                                                           [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)
                                                      (push-item input2 :input)))
                           result (top-item :integer final-state)]
                       (when print-outputs
                          (println (format "Correct output: %s | Program output: %s" (str correct-output) (str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is integer difference
                       (if (number? result)
                         (abs (- result correct-output)) ; distance from correct integer
                         1000000) ; penalty for no return value
                       )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-gcd-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map gcd-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def gcd-train-and-test-cases
  (get-gcd-train-and-test gcd-data-domains))

(defn gcd-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first gcd-train-and-test-cases))]
    (println (format "Train Case: %3s | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second gcd-train-and-test-cases))]
    (println (format "Test Case: %3s | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn gcd-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- GCD problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 1.0E-3)
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
  {:error-function (make-gcd-error-function-from-cases (first gcd-train-and-test-cases)
                                                                  (second gcd-train-and-test-cases))
   :atom-generators gcd-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
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
   :problem-specific-report gcd-report
   :problem-specific-initial-report gcd-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
