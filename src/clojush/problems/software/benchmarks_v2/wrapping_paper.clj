;; wrapping_paper.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem inspired by: https://adventofcode.com/2015/day/2

(ns clojush.problems.software.benchmarks-v2.wrapping-paper
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag)
  (:require [clojure.math.numeric-tower :as nt]
            clojure.string))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:string :char :integer :boolean :exec])
    (list (tag-instruction-erc [:string :char :integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list 0
         \x
         (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
         (fn [] (lrand-nth "0123456789")) ;Char digit ERC
         (fn [] (- (lrand-int 201) 100)) ;Integer ERC
         ) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(defn wrapping-paper-input
  "Creates a string input to Wrapping Paper."
  []
  (let [length (inc (lrand-int 30))
        width (inc (lrand-int 30))
        height (inc (lrand-int 30))]
    (format "%dx%dx%d" length width height)))

;; A list of data domains. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(list "1x1x1"
          "1x1x2"
          "1x2x1"
          "2x1x1"
          "3x2x1"
          "30x30x30"
          "1x30x30"
          "30x1x30"
          "30x30x1"
          "30x1x1"
          "5x5x5"
          "29x28x30"
          "9x30x9"
          "10x5x10"
          "2x20x20") 15 0] ; fixed inputs
   [wrapping-paper-input 185 2000] ; random strings, length [2, 20]
   ])

;;Can make test data like this:
(comment (test-and-train-data-from-domains data-domains))

(defn solve-wrapping-paper
  [input]
  (let [dims (clojure.string/split input #"x")
        w (Integer/parseInt (first dims))
        l (Integer/parseInt (second dims))
        h (Integer/parseInt (nth dims 2))
        wl (* w l)
        wh (* w h)
        lh (* l h)
        smallest (min wl wh lh)]
    (+ smallest
       (* 2 (+ wl wh lh)))))

; Helper function for error function
(defn test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (solve-wrapping-paper in)))
       inputs))

(defn make-error-function-from-cases
  "Creates and returns the error function based on the train/test cases."
  [train-cases test-cases]
  (fn the-actual-error-function
    ([individual]
      (the-actual-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input1 correct-output] (case data-cases
                                                              :train train-cases
                                                              :test test-cases
                                                              [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                        (push-item input1 :input)))
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %4d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer difference
                         (if (number? result)
                           (nt/abs (- result correct-output)) ; distance from correct integer
                           1000000) ; penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def train-and-test-cases
  (get-train-and-test data-domains))

(defn initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn custom-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Find Pair problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-error-function-from-cases (first train-and-test-cases)
                                                   (second train-and-test-cases))
   :atom-generators atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1.0}
   :uniform-addition-and-deletion-rate 0.09
   :problem-specific-report custom-report
   :problem-specific-initial-report initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000})
