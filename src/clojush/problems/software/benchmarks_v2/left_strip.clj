;; left_strip.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Problem inspired by: https://www.codewars.com/kata/580479d7357636a9ff000085

(ns clojush.problems.software.benchmarks-v2.left-strip
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag)
  (:require [clojure.math.numeric-tower :as nt]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:string :char :integer :boolean :exec])
    (list (tag-instruction-erc [:string :char :integer :boolean :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list 0
         "") ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(def left-strip-chars (map char (range 32 127)))

(defn left-strip-input-string
  "Creates a string of length len."
  [len]
  (apply str
         (repeatedly len
                     #(lrand-nth left-strip-chars))))

(defn left-strip-string-biased
  "Makes a string that a small percent of the time takes
   chars out of target."
  [len target]
  (apply str
         (repeatedly len
                     #(if (< (lrand) 0.1)
                        (lrand-nth target)
                        (lrand-nth left-strip-chars)))))

(defn left-strip-input
  "Creates a Left Strip problem input consisting of 2 strings."
  []
  (let [chars-to-strip (left-strip-input-string (inc (lrand-int 11)))
        len-text (if (< (lrand) 0.5)
                   (lrand-int 21) ; 1/2 time, rand string length [0,20]
                   (+ 10 (lrand-int 11))) ; 1/2 time, have string length > 10
        len-removal (lrand-int 21)
        ; This is really convoluted, but ensures that some of chars-to-strip
        ; may appear at the left side of text 
        text (apply str
                    (take len-text
                          (str
                           (apply str
                                  (take (lrand-int (inc len-text)) ; len-text here means the length of the stripped chars is influenced by length of text
                                        (shuffle
                                         (apply list
                                                (apply str (repeat 20 chars-to-strip))))))
                           (left-strip-string-biased len-text chars-to-strip))))
        ; This makes sure char-to-strip is in removal (if removal is long enough),
        ; but the chars in removal are shuffled
        removal (apply str
                       (shuffle
                        (take len-removal
                              (str chars-to-strip
                                   (left-strip-input-string 20)))))]
    [text removal]))

;; A list of data domains. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(list ["" ""]
          ["<" "<"]
          ["<" "]"]
          ["t" "nddgvo21rgif2tdvq"]
          ["z" "nddgvo21rgif2tdvq"]
          ["tttzzztttzzztttzzz" "nddgvo21rgif2tdvq"]
          ["" "abc 123"]
          ["abc 123" ""]
          ["abc 123" "abc"]
          ["abc 123" "cab"]
          ["abc 123" " "]
          ["abc 123" "123"]
          ["abc 123" "3c2b1a"]
          ["abc 123" "tabby cat"]
          ["     #    how #is #" " "]
          ["     #    how #is #" " #"]
          ["     #    how #is #" "# "]
          ["     #    how #is #" "osY"]
          ["tabby cat" "abc 123"]
          ["tabby cat" "abc 123 taco"]
          ["ABCDEFGHIJKLMNOPQRST" "TSRQPONMLKJIHGFEDCBA"]
          ["ABCDEFGHIJKLMNOPQRST" "TSRQPONMLKJIHGFEDCB"]
          ["ABCDEFGHIJKLMNOPQRST" "KLINDJTMRBAHCQPEGSFO"]
          ["5555555555555554535" ""]
          ["5555555555555554535" "5"]
          ["5555555555555554535" "45"]
          ["STRINGS NOT RELATED!" "do_you_want_tea?"]
          ["STRINGS NOT RELATED!" "DO_YOU_WANT_TEA?"]
          ["STRINGS ARE RELATED!" "T GRINS"]
          ["9#9#9#9#9#9#9" "hashtag 9"]) 30 0] ; fixed strings
   [left-strip-input 170 2000] ; random strings, length [0, 20]
   ])

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn solve-left-strip
  "Solves the problem given the input."
  [[text removals]]
  (loop [answer text]
    (cond
      (empty? answer) answer
      (not (in? removals (first answer))) answer
      :else (recur (apply str (rest answer))))))

; Helper function for error function
(defn test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (solve-left-strip in)))
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
