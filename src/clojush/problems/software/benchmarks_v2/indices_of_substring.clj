;; indices_of_substring.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem inspired by https://www.codewars.com/kata/56b78faebd06e61870001191

(ns clojush.problems.software.benchmarks-v2.indices-of-substring
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag)
  (:require [clojure.math.numeric-tower :as nt]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:vector_integer :string :char :integer :boolean :exec])
    (list (tag-instruction-erc [:vector_integer :string :char :integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1 'in2) ; inputs
   (list 0
         1
         []
         "") ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(def substring-indices-chars (map char (range 32 127)))

(defn text-from-target
  "Creates a text from a target string and a length"
  [len target]
  (let [prob-of-target (* 0.8 (lrand))
        long-text (apply str
                         (repeatedly len
                                     #(if (< (lrand) prob-of-target)
                                        target
                                        (lrand-nth substring-indices-chars))))
        text (apply str (take len long-text))]
    text))

(defn substring-indices-input
  "Creates a text string of length len and a target string."
  [len]
  (let [target-length (lrand-nth [1 1 1 2 2 2 2 3 3 3 3 4 4 5 6])
        target (apply str
                      (repeatedly target-length 
                                  #(lrand-nth substring-indices-chars)))
        text (text-from-target len target)]
    [text target]))

;; A list of data domains. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(list
     ["a" "5"]
     ["!" "!"]
     ["r" "nm,xcnwqnd@#$fwkdjn3"]
     ["hi" "hihihihihihihihihihi"]
     ["############" "#"]
     ["GGGGGGGGGGGGGGGGGGGG" "G"]
     ["$$$$$$$$$$$$$$$$$$$$" "$$"]
     ["33333333333333333333" "333"] ; these are important for overlap
     ["hahahahahahahahahaha" "haha"]
     ["GCTGCTGCTGCTGCTGCTGC" "GCTGC"]
     ["bbbbbbb(bb#bbbbbbbb" "bbb"]
     ["fa la la la la, la " "la"]
     ["start and and with s" "s"]
     ["tomato" "tom"]
     ["tomatotomatotomato" "tom"]
     ["tomatotomatotomato" "to"]
     ["will be zero" "this will be zero"]
     ["APPEAR twice APPEAR" "APPEAR"]
     ["a few ending <3<3<3" "<3"]
     ["middle of this one" "of"])
    20 0]
   [(fn [] (substring-indices-input (inc (lrand-int 10)))) 40 500] ; A smaller number of shorter inputs
   [(fn [] (substring-indices-input (+ 11 (lrand-int 10)))) 140 500] ; A larger number of longer inputs
   ])

;;Can make test data like this:
(comment (test-and-train-data-from-domains data-domains))

(defn solve-substring-indices
  "Solves the problem given the input."
  [[text target]]
  (vec (filter #(let [end (min (count text)
                               (+ % (count target)))]
                  (= (subs text % end) target))
               (range (count text)))))

; Helper function for error function
(defn test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (solve-substring-indices in)))
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
                   (for [[[input1 input2] correct-output] (case data-cases
                                                            :train train-cases
                                                            :test test-cases
                                                            data-cases)]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input2 :input)
                                                      (push-item input1 :input)))
                           result (top-item :vector_integer final-state)]
                       (when print-outputs
                         (println (format "| Correct output: %s\n| Program output: %s\n" (str correct-output) (str result))))
                         ; Record the behavior
                       (swap! behavior conj result)
                         ; Error is Levenshtein distance
                       (if (coll? result)
                         (levenshtein-distance correct-output result)
                         1000000) ; penalty for no return value
                       )))]
       (if (= data-cases :test)
         (assoc individual :test-errors errors)
         (assoc individual
                :behaviors (reverse @behavior)
                :errors errors))))))

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
   :training-cases (first train-and-test-cases)
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
