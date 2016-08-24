;; grade.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given 5 integer inputs, all in range [0,100]. The first four represent the
;; lower numeric thresholds for achieving an A, B, C, and D, and will be
;; distinct and in descending order. The fifth represents the student's numeric
;; grade. The program must output "Student has a X grade.", where X is A, B, C,
;; D, or F depending on the thresholds and their numeric grade.
;;
;; input stack has 5 integers

(ns clojush.problems.software.grade
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def grade-atom-generators
  (concat (list
            "Student has a "
            " grade."
            "A"
            "B"
            "C"
            "D"
            "F"
            ;;; end constants
            (fn [] (lrand-int 101)) ;Integer ERC [0,100]
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :string :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tagERCs
            'in1
            'in2
            'in3
            'in4
            'in5
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :exec :print])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def grade-data-domains
  [[(list '(80 70 60 50 85)
          '(80 70 60 50 80)
          '(80 70 60 50 79)
          '(80 70 60 50 75)
          '(80 70 60 50 70)
          '(80 70 60 50 69)
          '(80 70 60 50 65)
          '(80 70 60 50 60)
          '(80 70 60 50 59)
          '(80 70 60 50 55)
          '(80 70 60 50 50)
          '(80 70 60 50 49)
          '(80 70 60 50 45)) 13 0] ;; Cases from Repair Benchmark paper
   [(list '(90 80 70 60 100)
          '(90 80 70 60 0)
          '(4 3 2 1 5)
          '(4 3 2 1 4)
          '(4 3 2 1 3)
          '(4 3 2 1 2)
          '(4 3 2 1 1)
          '(4 3 2 1 0)
          '(100 99 98 97 100)
          '(100 99 98 97 99)
          '(100 99 98 97 98)
          '(100 99 98 97 97)
          '(100 99 98 97 96)
          '(98 48 27 3 55)
          '(98 48 27 3 14)
          '(98 48 27 3 1)
          '(45 30 27 0 1)
          '(45 30 27 0 0)
          '(48 46 44 42 40)
          '(48 46 44 42 41)
          '(48 46 44 42 42)
          '(48 46 44 42 43)
          '(48 46 44 42 44)
          '(48 46 44 42 45)
          '(48 46 44 42 46)
          '(48 46 44 42 47)
          '(48 46 44 42 48)
          '(48 46 44 42 49)) 28 0] ;; Hand-written cases
   [(fn []
      (let [thresholds (sort > (repeatedly 4 #(lrand-int 101)))]
        (if (apply distinct? thresholds)
          (concat thresholds (list (lrand-int 101)))
          (recur)))) 159 2000] ;; Random cases, which make sure that first 4 integers are distinct
   ])

;;Can make Grade test data like this:
;(test-and-train-data-from-domains grade-data-domains)

; Helper function for error function
(defn grade-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2 input3 input4 input5] output]."
  [inputs]
  (map #(vector %
                (str "Student has a "
                     (let [score (last %)]
                       (cond
                         (>= score (first %)) \A
                         (>= score (second %)) \B
                         (>= score (nth % 2)) \C
                         (>= score (nth % 3)) \D
                         :else \F))
                     " grade."))
       inputs))

(defn make-grade-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-grade-error-function
    ([program]
      (the-actual-grade-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-grade-error-function program data-cases false))
    ([program data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                     (doall
                       (for [[[input1 input2 input3 input4 input5] correct-output] (case data-cases
                                                                                     :train train-cases
                                                                                     :test test-cases
                                                                                     [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input5 :input)
                                                       (push-item input4 :input)
                                                       (push-item input3 :input)
                                                       (push-item input2 :input)
                                                       (push-item input1 :input)
                                                       (push-item "" :output)))
                               printed-result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "Correct output: %-19s | Program output: %-19s" (pr-str correct-output) (pr-str printed-result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj printed-result))
                           ; Error is Levenshtein distance and, if correct format, distance from correct letter grade character
                           (vector
                             (levenshtein-distance correct-output printed-result)
                             (let [printed-letter (second (re-find #"^Student has a (.) grade.$" printed-result))
                                   correct-letter (second (re-find #"^Student has a (.) grade.$" correct-output))]
                               (if printed-letter
                                 (abs (- (int (first correct-letter))
                                         (int (first printed-letter)))) ;distance from correct character
                                 1000))
                             )))))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-grade-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map grade-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def grade-train-and-test-cases
  (get-grade-train-and-test grade-data-domains))

(defn grade-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first grade-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second grade-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn grade-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Grade problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-grade-error-function-from-cases (first grade-train-and-test-cases)
                                                         (second grade-train-and-test-cases))
   :atom-generators grade-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 800
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :epigenetic-markers [:close]
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report grade-report
   :problem-specific-initial-report grade-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
