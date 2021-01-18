;; character_filter.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem inspired by Hamilton College CS 101 lab problem

(ns clojush.problems.software.benchmarks-v2.character-filter
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
         "") ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(def character-filter-chars (map char (range 32 127)))

(defn insert-at-random-index
  "Inserts element at a random index in coll."
  [coll element]
  (let [index (lrand-int (inc (count coll)))]
    (concat (take index coll)
            (list element)
            (drop index coll))))

(defn character-filter-random-string
  "Creates a string for this problem.
   the-char-or-nil is either a character to include in the string or nil"
  [the-char-or-nil]
  (let [len-str (+ (lrand-int 5)
                   (lrand-int 5))
        some-chars (repeatedly len-str #(lrand-nth character-filter-chars))]
    (if the-char-or-nil
      (apply str (insert-at-random-index some-chars the-char-or-nil))
      (apply str some-chars))))

(defn character-filter-input
  "Creates a vector of strings of length len and a character.
   Character is chosen first.
   Then, a probability in [0, 1] is uniformly chosen.
   This probability is used to randomize if the given character is included
   in each string."
  [len]
  (let [the-char (lrand-nth character-filter-chars)
        char-probability (lrand)]
    (loop [strings []]
      (cond
        (>= (count strings) len) [strings the-char]
        (< (lrand) char-probability) (recur (conj strings (character-filter-random-string the-char)))
        :else (recur (conj strings (character-filter-random-string nil)))))))

;; A list of data domains. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(list
     [["hi" "there" "world" "of" "things" "and" "stuff"] \h]
     [["aaaaabbb" "aaaaccc" "aaaddd" "aaeee" "afff" "ggg" "a" "hhhhhhhh"] \a]
     [["This" "one cares" "about SPACES" "and" "nothing more" "how about" "THAT"] \space]
     [["123" "456" "789" "101112" "131415" "161718" "192021" "222324" "252627" "282930"] \1]
     [["This" "one" "results" "in" "an" "empty" "string."] \Z]
     [["~!@#$%^&*()"] \$]
     [["~!@#$%^&*()"] \<]
     [["" "" "" "" "" ""] \w] ; weird one, but needs to handle it
     [["CAPSLOCK" "IS" "STUCK" "ON" "SOMETIMES!!!" "BUT" "THAT'S" "OK."] \S]
     [["ASD" "efe" "brq" "3432f" "@#R" "d2d2" "ee2" "fvds" "c w" "112" "!$%" "@^3" "vvr" "&*(" "MOO" ";']" "Q!~" "xnw" "   " "d21"] \@])
    10 0]
   [(fn [] (character-filter-input 0)) 5 0] ; length-0 vectors
   [(fn [] (character-filter-input 1)) 20 0] ; length-1 vectors
   [(fn [] (character-filter-input 2)) 20 0] ; length-2 vectors
   [(fn [] (character-filter-input (inc (lrand-int 20)))) 145 2000]])

;;Can make test data like this:
(comment (test-and-train-data-from-domains data-domains))

(defn solve-character-filter
  "Solves the problem given the input."
  [[vec-of-str the-char]]
  (apply str (filter #(some #{the-char} %)
                     vec-of-str)))

; Helper function for error function
(defn test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (solve-character-filter in)))
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
