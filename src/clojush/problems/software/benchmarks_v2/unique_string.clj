;; unique_string.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem inspired by https://www.codewars.com/kata/585d7d5adb20cf33cb000235

(ns clojush.problems.software.benchmarks-v2.unique-string
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag)
  (:require [clojure.math.numeric-tower :as nt]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:vector_string :string :integer :boolean :exec])
    (list (tag-instruction-erc [:vector_string :string :integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list 0
         1
         []
         "") ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(def unique-string-chars (map char (range 32 127)))

(defn random-string
  "Makes a random string of length len"
  [len]
  (apply str (repeatedly len
                         #(lrand-nth unique-string-chars))))

(defn unique-string-input
  "Creates a vector of strings of length len, where all strings are
   identical except for one.
   Each string is of length [0, 10]"
  [len]
  (let [repeated-string (random-string (lrand-int 11))
        the-vec (vec (repeat len repeated-string))
        unique-string (first (filter #(not= % repeated-string)
                                     (repeatedly #(random-string (lrand-int 11)))))]
    (assoc the-vec
           (lrand-int len)
           unique-string)))

;; A list of data domains. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(list
     ["" "" "hello"]
     ["" "DOG" ""]
     ["123456789" "" ""]
     ["" "over9000" "over9000"]
     ["computer" "" "computer"]
     ["SCIENCE" "SCIENCE" ""]
     ["abc" "abc" "abc" "987"]
     ["4" "44" "44" "44"]
     ["small DIFF" "small DIFF" "smallDIFF" "small DIFF"]
     ["Practice" "Practice" "Practice" "Practice" "Practice" "Practice"
      "Practice" "Practice" "Practice" "Practice" "Practice" "PRACTICE"
      "Practice" "Practice" "Practice" "Practice" "Practice" "Practice"
      "Practice" "Practice"])
    10 0]
   [(fn [] (unique-string-input 3)) 20 0] ; length-3 vectors
   [(fn [] (unique-string-input 4)) 20 0] ; length-4 vectors
   [(fn [] (unique-string-input (+ 3 (lrand-int 18)))) 150 2000]])

;;Can make test data like this:
(comment (test-and-train-data-from-domains data-domains))

(defn solve-unique-string
  "Solves the problem given the input."
  [vec-of-str]
  (let [the-repeat (cond
                     (= (first vec-of-str)
                        (second vec-of-str)) (first vec-of-str)
                     (= (first vec-of-str)
                        (nth vec-of-str 2)) (first vec-of-str)
                     :else (second vec-of-str))
        answer (first (filter #(not= % the-repeat)
                              vec-of-str))]
    (if (nil? answer)
      (throw (Exception. (str "The following input vector to the Unique String
problem does not seem to have a unique string in it:" vec-of-str)))
      answer)))

; Helper function for error function
(defn test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (solve-unique-string in)))
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
                   (for [[input correct-output] (case data-cases
                                                  :train train-cases
                                                  :test test-cases
                                                  [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input :input)))
                           result (top-item :string final-state)]
                       (when print-outputs
                         (println (format "| Correct output: %s\n| Program output: %s\n" (str correct-output) (str result))))
                         ; Record the behavior
                       (swap! behavior conj result)
                         ; Error is right/wrong; no penalty for no output besides wrong
                       (if (= result correct-output)
                         0
                         1)
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
    (printf ";; -*- Find Pair problem report - generation %s\n" generation) (flush)
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
    (println ";;******************************"))) ;; To do validation, could have this function return an altered best individual
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
   :max-error 1})
