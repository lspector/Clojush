;; for_loop_index.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given 3 integer inputs (start, finish, stepSize), print the integers
;; represented by the Java for loop:
;;      for(i = start; i < finish; i += stepSize) System.out.println(i);
;;
;; Note that start < finish for all test cases, so will always require printing something.
;;
;; Note: tried adding extra error, did not help
;;
;; input stack has 3 input integers: in1 = start, in2 = finish, in3 = step-size

(ns clojush.problems.software.for-loop-index
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def loop-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :print])))


;; Define test cases
(defn loop-input
  "Makes a For Loop Index input vector = [start, finish, step-size] with start < finish."
  [& {:keys [neg-pos] :or {neg-pos false}}]
  (if (not neg-pos)
    (let [step-size (inc (lrand-int 10))
          start (- (lrand-int 1000) 500)
          finish (+ start 1 (lrand-int (* 20 step-size)))]
      [start finish step-size])
    (let [step-size (inc (lrand-int 10))
          start (dec (- (lrand-int (* 10 step-size))))
          finish (lrand-int (* 10 step-size))]
      [start finish step-size])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def loop-data-domains
  [[(fn [] (loop-input :neg-pos true)) 10 100] ;; Cases where start < 0 and finish > 0
   [(fn [] (loop-input)) 90 900] ;; Random cases
   ])

;;Can make For Loop Index test data like this:
;(test-and-train-data-from-domains loop-data-domains)

; Helper function for error function
(defn loop-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (apply str (interpose \newline (apply range %))))
       inputs))

(defn make-for-loop-index-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-loop-error-function
    ([program]
      (the-actual-loop-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-loop-error-function program data-cases false))
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
                                                     (push-item input1 :input)
                                                     (push-item "" :output)))
                             result (stack-ref :output 0 final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (when @global-print-behavioral-diversity
                           (swap! behavior conj result))
                         ; Error is Levenshtein distance of printed strings
                         (levenshtein-distance correct-output result))))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-for-loop-index-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map loop-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def for-loop-index-train-and-test-cases
  (get-for-loop-index-train-and-test loop-data-domains))

(defn for-loop-index-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first for-loop-index-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second for-loop-index-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn loop-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- For Loop Index problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-for-loop-index-error-function-from-cases (first for-loop-index-train-and-test-cases)
                                                                  (second for-loop-index-train-and-test-cases))
   :atom-generators loop-atom-generators
   :max-points 600
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
   :problem-specific-report loop-report
   :problem-specific-initial-report for-loop-index-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
