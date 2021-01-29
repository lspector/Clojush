;; nth_concat.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Note that this problem is harder for clojush because string_nth finds the
;; integer mod the size of the string to get the index.
;;
;; Problem inspired by Hamilton College CS 101 lab problem

(ns clojush.problems.software.benchmarks-v2.nth-concat
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag)
  (:require [clojure.math.numeric-tower :as nt]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:vector_string :string :char :integer :boolean :exec])
    (list (tag-instruction-erc [:vector_string :string :char :integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1 'in2) ; inputs
   (list []
         ""
         0
         1
         ) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(def nth-concat-chars (map char (range 32 127)))

(defn nth-concat-random-string
  "Creates a string for this problem of length len"
  [len]
  (apply str (repeatedly len #(lrand-nth nth-concat-chars))))

(defn rand-int-range
  "Returns an integer between start (inclusive) and end (exclusive)"
  [start end]
  (+ start (lrand-int (- end start))))

(defn nth-concat-input-all-have-n
  "Creates a vector of strings of length len and an index n in [0,19].
   This version makes sure all strings have length > n, so that all
   strings will contribute to answer"
  [len]
  (let [n (lrand-int 20)]
    (vector (vec (repeatedly len #(nth-concat-random-string
                                   (rand-int-range (inc n) 21))))
            n)))

(defn nth-concat-input-some-shorter-than-n
  "Creates a vector of strings of length len and an index n in [0,20].
   Note that if n = 20, it will always be outside of the indices of the strings.
   This version allows strings of any length [0,20], so that some will not
   have index n in them."
  [len]
  (let [n (lrand-int 21)]
    (vector (vec (repeatedly len #(nth-concat-random-string
                                   (lrand-int 21))))
            n)))

;; A list of data domains. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(list [[] 5]
          [[] 20]
          [["arbitrary string"] 8]
          [["01234567890123456789"] 19]
          [["a" "b" "c" "d" "e" "f" "g"] 0]
          [["12" "34" "56" "78" "90"] 1]
          [["12ab" "34cd" "56ef" "78gh" "90ij"] 1]
          [["hi" "there" "world" "is" "nice" "!"] 0]
          [["POIUYTREWQLKJHGFDSAM"
            "zxcvbnmasdfghjklqwer"
            "~!@#$%^&*()_+-=[];'."
            "0987654321poiuytNBVC"] 19]
          [["cat" "SUNNY" "cat" "SUNNY" "cat" "SUNNY" "cat" "SUNNY" "cat" "SUNNY"
            "dog" "HAROLD" "dog" "HAROLD" "dog" "HAROLD" "dog" "HAROLD" "dog" "HAROLD"] 2])
    10 0] ; inputs where n is within the bounds of every string in the vector
   [(list [["friend"] 10]
          [["01234567890123456789"] 20]
          [["its in" "out"] 3]
          [["too short" "this one is longer"] 16]
          [["one long string here" "a bit shorter" "also pretty long"] 14]
          [["G" "" "O" "" "A" "" "T" ""] 0]
          [["hi" "there" "world" "is" "nice" "!"] 1]
          [["hi" "there" "world" "is" "nice" "!"] 2]
          [["hi" "there" "world" "is" "nice" "!"] 3]
          [["hi" "there" "world" "is" "nice" "!"] 4]
          [["hi" "there" "world" "is" "nice" "!"] 5]
          [["hi" "there" "world" "is" "nice" "!"] 20]
          [["" "1" "23" "456" "789q" "werty" "uiop[]" "asdfghj" "kl;'zxcv" "bnm,./<>?"] 0]
          [["" "1" "23" "456" "789q" "werty" "uiop[]" "asdfghj" "kl;'zxcv" "bnm,./<>?"] 5]
          [["" "1" "23" "456" "789q" "werty" "uiop[]" "asdfghj" "kl;'zxcv" "bnm,./<>?"] 8]
          [["" "1" "23" "456" "789q" "werty" "uiop[]" "asdfghj" "kl;'zxcv" "bnm,./<>?"] 15]
          [["POIUYTREWQLKJHGFDSAM"
            "zxcvbnmasdfghjklqwer"
            "~!@#$%^&*()_+-=[];'."
            "0987654321poiuytNBVC"] 20]
          [["cat" "SUNNY" "cat" "SUNNY" "cat" "SUNNY" "cat" "SUNNY" "cat" "SUNNY"
            "dog" "HAROLD" "dog" "HAROLD" "dog" "HAROLD" "dog" "HAROLD" "dog" "HAROLD"] 3]
          [(vec (repeat 20 "")) 12]
          [["" "456" "" "" "" "321" "" "" "train" "" "      " "QUACK" "" "" "" "" "ken" ""] 1]         
          )
    20 0] ; inputs where n is outside the bounds of some strings
   [(fn [] (nth-concat-input-all-have-n (rand-int-range 1 21))) 50 1000] ; all indices are in bounds
   [(fn [] (nth-concat-input-some-shorter-than-n (rand-int-range 1 21))) 120 1000]] ; some indices may be out of bounds
)

;;Can make test data like this:
(comment (test-and-train-data-from-domains data-domains))

(defn solve-nth-concat
  "Solves the problem given the input."
  [[vec-of-str n]]
  (apply str
         (map (fn [string]
                (if (< n (count string))
                  (nth string n)
                  ""))
              vec-of-str)))

; Helper function for error function
(defn test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (solve-nth-concat in)))
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
                                                            [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input2 :input)
                                                      (push-item input1 :input)))
                           result (top-item :string final-state)]
                       (when print-outputs
                         (println (format "| Correct output: %s\n| Program output: %s\n" (str correct-output) (str result))))
                         ; Record the behavior
                       (swap! behavior conj result)
                         ; Error is Levenshtein distance
                       (if (string? result)
                         (levenshtein-distance correct-output (str result))
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
   :max-error 1000000})
