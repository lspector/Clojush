;; roman_numerals.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)

(ns clojush.problems.software.roman-numerals
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

(def roman-numeral-values
  (let [roman-numeral-map {\i 1
                      \v 5
                      \x 10
                      \l, 50
                      \c 100
                      \d 500
                      \m 1000}
        visible-chars (map char (range 0 127))]
    (vec (for [c visible-chars]
           (get roman-numeral-map (first (string/lower-case c)) 0)))))

; Atom generators
(def roman-numerals-atom-generators
  (concat (list
            roman-numeral-values
            1
            5
            10
            50
            100
            500
            1000
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :vector_integer :exec])))

;; Define test cases
(defn roman-numerals-input
 "Makes a Roman Numeral input given a decimal number."
 [num]
 (apply str
           (loop [roman "" number num]
             (cond
               (> number (mod number 1000)) (recur (str roman "M") (- number 1000))
               (> number (mod number 900)) (recur (str roman "CM") (- number 900))
               (> number (mod number 500)) (recur (str roman "D") (- number 500))
               (> number (mod number 400)) (recur (str roman "CD") (- number 400))
               (> number (mod number 100)) (recur (str roman "C") (- number 100))
               (> number (mod number 90)) (recur (str roman "XC") (- number 90))
               (> number (mod number 50)) (recur (str roman "L") (- number 50))
               (> number (mod number 40)) (recur (str roman "XL") (- number 40))
               (> number (mod number 10)) (recur (str roman "X") (- number 10))
               (> number (mod number 9)) (recur (str roman "IX") (- number 9))
               (> number (mod number 5)) (recur (str roman "V") (- number 5))
               (> number (mod number 4)) (recur (str roman "IV") (- number 4))
               (> number (mod number 1)) (recur (str roman "I") (- number 1))
               :else roman))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def roman-numerals-data-domains
  [[(list ""
          "I"
          "V"
          "X"
          "L"
          "C"
          "D"
          "M"
          "IV"
          "VI"
          "IX"
          "XI"
          "MMMCMXCIX") 13 0] ; Special edge cases
    [(fn [] (roman-numerals-input (inc (lrand-int 3999)))) 187 2000]
   ;[(fn [] (map roman-numerals-input (remove #(contains? #{1 5 10 50 100 500 1000 4 6 9 11 3999} %) (range 1 4000)))) 0 3987] ; The rest of the possible numbers 1-3999
   ])

;;Can make Roman Numeral test data like this:
;(test-and-train-data-from-domains roman-numerals-data-domains)

; Helper function for error function
(defn roman-numerals-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (println inputs)
  (map (fn [in]
          (vector in
              (loop [roman in number 0]
                (cond
                  (= (take 2 roman) '(\I \V)) (recur (drop 2 roman) (+ number 4))
                  (= (take 2 roman) '(\I \X)) (recur (drop 2 roman) (+ number 9))
                  (= (take 2 roman) '(\X \L)) (recur (drop 2 roman) (+ number 40))
                  (= (take 2 roman) '(\X \C)) (recur (drop 2 roman) (+ number 90))
                  (= (take 2 roman) '(\C \D)) (recur (drop 2 roman) (+ number 400))
                  (= (take 2 roman) '(\C \M)) (recur (drop 2 roman) (+ number 900))
                  (= (first roman) \M) (recur (drop 1 roman) (+ number 1000))
                  (= (first roman) \D) (recur (drop 1 roman) (+ number 500))
                  (= (first roman) \C) (recur (drop 1 roman) (+ number 100))
                  (= (first roman) \L) (recur (drop 1 roman) (+ number 50))
                  (= (first roman) \X) (recur (drop 1 roman) (+ number 10))
                  (= (first roman) \V) (recur (drop 1 roman) (+ number 5))
                  (= (first roman) \I) (recur (drop 1 roman) (+ number 1))
                  :else number))))
       inputs))

(defn make-roman-numerals-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-roman-numerals-error-function
    ([individual]
      (the-actual-roman-numerals-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-roman-numerals-error-function individual data-cases false))
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
                             result (stack-ref :integer 0 final-state)]
                         (when print-outputs
                           (println (format "Correct output: %3d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is difference of integers
                         (if (number? result)
                           (abs (- result correct-output)) ;distance from correct integer
                           100000) ;penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-roman-numerals-train-and-test
  "Returns the train and test cases."
  [data-domains]
     (map roman-numerals-test-cases
          (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def roman-numerals-train-and-test-cases
  (get-roman-numerals-train-and-test roman-numerals-data-domains))

(defn roman-numerals-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first roman-numerals-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second roman-numerals-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn roman-numerals-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Roman Numeral problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-roman-numerals-error-function-from-cases (first roman-numerals-train-and-test-cases)
                                                                  (second roman-numerals-train-and-test-cases))
   :atom-generators roman-numerals-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 3000
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
   :problem-specific-report roman-numerals-report
   :problem-specific-initial-report roman-numerals-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
