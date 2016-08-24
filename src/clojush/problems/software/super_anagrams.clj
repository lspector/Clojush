;; super_anagrams.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given strings x and y of lowercase letters with length <= 20, return true if
;; y is a super anagram of x, which is the case if every character in x is in y.
;; To be true, y may contain extra characters, but must have at least as many
;; copies of each character as x does.
;;
;; input stack has the 2 input strings
;;

(ns clojush.problems.software.super-anagrams
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

; Atom generators
(def super-anagrams-atom-generators
  (concat (list
            ;;; end constants
            (fn [] (lrand-nth (list true false))) ;Boolean ERC
            (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC [-1000,1000]
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))


;; Define test cases
(defn super-anagrams-input
  "Makes a pair of Super Anagrams inputs."
  []
  (let [len (inc (lrand-int 20))
        input1 (apply str
                      (repeatedly len
                                  (fn []
                                    (lrand-nth (map char (range 97 123))))))
        change-char-sometimes-fn (fn [c]
                                   "Each char has 10% chance of being replaced"
                                   (if (< (lrand) 0.1)
                                     (lrand-nth (map char (range 97 123)))
                                     c))
        num-chars-to-drop (lrand-int len)
        input2 (apply str (drop num-chars-to-drop
                                (shuffle (map change-char-sometimes-fn input1))))]
    (if (< (lrand) 0.2) ;;Choose random order (biased towad input2 first) since len(input1) >= len(input2)
      [input1 input2]
      [input2 input1])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def super-anagrams-data-domains
  [[(list ["" ""]
          ["" "h"]
          ["i" ""]
          ["a" "a"]
          ["b" "c"]
          ["n" "nn"]
          ["abcde" "c"]
          ["c" "abcde"]
          ["r" "mnbvccxz"]
          ["abc" "aabc"]
          ["aabc" "abcde"]
          ["abcde" "edcba"]
          ["mo" "moo"]
          ["moo" "mo"]
          ["tree" "though"]
          ["rip" "zipper"]
          ["flipper" "rip"]
          ["hi" "zipper"]
          ["dealer" "dollars"]
          ["loud" "louder"]
          ["ccccccccc" "ccccc"]
          ["clinteastwood" "oldwestaction"]
          ["clinteastwood" "ldwestaction"]
          ["verificationcomplete" "verificationcomplete"]
          ["hahahahahahahahahaha" "hhhhhhhhhhaaaaaaaaaa"]
          ["hahahahahahahahahaha" "aahhhh"]
          ["" "qwqeqrqtqyquqiqoqpqs"]
          ["wxyz" "qazwsxedcrfvtgbyhnuj"]
          ["dddeeefffgggg" "gggffggfefeededdd"]
          ["gggffggfefeededdd" "dddeeefffgggg"]) 30 0] ; hand-written cases
   [super-anagrams-input 170 2000] ; Random inputs designed to be close to anagrams
   ])

;;Can make test data like this:
;(test-and-train-data-from-domains super-anagrams-data-domains)

; Helper function for error function
(defn super-anagrams-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
         (vector [in1 in2]
                 (loop [i1 in1
                        i2 in2]
                   (cond
                     (empty? i1) true
                     (> 0 (.indexOf i2 (str (first i1)))) false
                     :else (recur (rest i1)
                                  (string/replace-first i2 (first i1) \space))))))
       inputs))

(defn make-super-anagrams-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-super-anagrams-error-function
    ([program]
      (the-actual-super-anagrams-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-super-anagrams-error-function program data-cases false))
    ([program data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1 input2] correct-output] (case data-cases
                                                              :train train-cases
                                                              :test test-cases
                                                              [])]
                       (let [final-state (run-push program
                                                   (->> (make-push-state)
                                                     (push-item input2 :input)
                                                     (push-item input1 :input)
                                                     (push-item "" :output)))
                             result (top-item :boolean final-state)]
                         (when print-outputs
                           (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (when @global-print-behavioral-diversity
                           (swap! behavior conj result))
                         ; Error is boolean error
                         (if (= result correct-output)
                           0
                           1))))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-super-anagrams-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by second %)
       (map super-anagrams-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def super-anagrams-train-and-test-cases
  (get-super-anagrams-train-and-test super-anagrams-data-domains))

(defn super-anagrams-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first super-anagrams-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second super-anagrams-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn super-anagrams-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Super Anagrams problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-super-anagrams-error-function-from-cases (first super-anagrams-train-and-test-cases)
                                                                  (second super-anagrams-train-and-test-cases))
   :atom-generators super-anagrams-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 1600
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
   :problem-specific-report super-anagrams-report
   :problem-specific-initial-report super-anagrams-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1
   })
