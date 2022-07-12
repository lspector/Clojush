;; substitution_cipher.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem inspired by: https://www.codewars.com/kata/52eb114b2d55f0e69800078d

(ns clojush.problems.psb2.substitution-cipher
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :exec :char :string]) ; stacks
    (list (tag-instruction-erc [:integer :boolean :exec :char :string] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1 'in2 'in3) ; inputs
   (list ""
         0) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

;; Define test cases
(defn substitution-cipher-input
  "Makes Substitution Cipher inputs 1 and 2 of length len-map and input 3 of
  len-code."
  [len-map len-code]
  (let [available-map "abcdefghijklmnopqrstuvwxyz"
        map1 (take len-map (shuffle (apply list available-map)))
        map2 (take len-map (shuffle (apply list available-map)))]
    (vector
     (apply str map1)
     (apply str map2)
     (apply str (repeatedly len-code #(rand-nth map1))))))

; A list of data domains for the problem. Each domain is a vector containing
; a "set" of inputs and two integers representing how many cases from the set
; should be used as training and testing cases respectively. Each "set" of
; inputs is either a list or a function that, when called, will create a
; random element of the set.
(def data-domains
  [[(list ["" "" ""]
          ["a" "a" "a"]
          ["j" "h" "j"]
          ["a" "z" "a"]
          ["e" "l" "eeeeeeeeee"]
          ["h" "d" "hhhhhhhhhhhhhhhhhhhh"]
          ["o" "z" "oooooooooooooooooooooooooo"]
          ["abcdefghijklmnopqrstuvwxyz" "zyxwvutsrqponmlkjihgfedcba" "bvafvuqgjkkbeccipwdfqttgzl"]
          ["abcdefghijklmnopqrstuvwxyz" "cdqutzayxshgfenjowrkvmpbil" "thequickbrownfxjmpsvlazydg"]
          ["otghvwmkclidzryxsfqeapnjbu" "alpebhxmnrcyiosvtgzjwuqdfk" "aaabbbccc"]) 10 0] ; "Special" inputs covering most base cases.
   [(fn [] (substitution-cipher-input (inc (lrand-int 26))
                                      (rand-int 27))) 190 2000]])

; Helper function for error function
(defn create-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2 input3] output]."
  [inputs]
  (map (fn [[in1 in2 in3]]
         (vector [in1 in2 in3]
                 (apply str (map (zipmap in1 in2)
                                 (map char in3)))))
       inputs))

(defn make-error-function-from-cases
  "Creates and returns the error function based on the train/test cases."
  [train-cases test-cases]
  (fn the-actual-error-function
    ([individual]
     (the-actual-error-function individual :train))
    ([individual data-cases] ; data-cases should be :train or :test
     (the-actual-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (flatten
                   (doall
                    (for [[[input1 input2 input3] correct-output] (case data-cases
                                                                    :train train-cases
                                                                    :test test-cases
                                                                    data-cases)]
                      (let [final-state (run-push (:program individual)
                                                  (->> (make-push-state)
                                                       (push-item input1 :input)
                                                       (push-item input2 :input)
                                                       (push-item input3 :input)))
                            result (top-item :string final-state)]
                        (when print-outputs
                          (println (format "| Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                           ; Record the behavior
                        (swap! behavior conj result)
                           ; Error is Levenshtein distance
                        (if (string? result)
                          (levenshtein-distance correct-output (str result))
                          10000) ; penalty for no return value
                        ))))]
       (if (= data-cases :test)
         (assoc individual :test-errors errors)
         (assoc individual
                :behaviors (reverse @behavior)
                :errors errors))))))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map create-test-cases
                 (test-and-train-data-from-domains data-domains))))

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
    (printf ";; -*- Substitution Cipher problem report - generation %s\n" generation) (flush)
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
    )) ; To do validation, could have this function return an altered best individual
       ; with total-error > 0 if it had error of zero on train but not on validation
       ; set. Would need a third category of data cases, or a defined split of training cases.


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
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5}
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report custom-report
   :problem-specific-initial-report initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 10000})