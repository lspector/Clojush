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
  [min-length]
  (let [len (+ min-length (lrand-int (- 11 min-length)))
        input1 (apply str
                      (repeatedly len
                                  (fn []
                                    (lrand-nth (map char (range 33 127))))))
        change-char-sometimes-fn (fn [c]
                                   "Each char has 40% chance of being replaced"
                                   (if (< (lrand) 0.4)
                                     (lrand-nth (map char (range 33 127)))
                                     c))
        num-chars-to-drop-last (lrand-nth (list 0 0 0 1 1 (lrand-int (inc (count input1)))))
        input2 (apply str (drop-last num-chars-to-drop-last
                                     (map change-char-sometimes-fn input1)))]
    (if (< (lrand) 0.5) ;;Choose random order since len(input1) >= len(input2)
      [input1 input2]
      [input2 input1])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def super-anagrams-data-domains
  [[(list ["" ""]
          ["" "hi"]
          ["hereworld" ""]
          ["a" "a"]
          ["b" "c"]
          ["n" "nn"]
          ["abcde" "c"]
          ["c" "abcde"]
          ["abc" "aabc"]
          ["aabc" "abcde"]
          ["abcde" "edcba"]
          ["mo" "moo"]
          ["moo" "mo"]
          ["rip" "zipper"]
          ["flipper" "rip"]
          ["hi" "zipper"]
          ["dealer" "dollars"]
          ["loud" "louder"]
          ["clinteastwood" "oldwestaction"]
          ["clinteastwood" "ldwestaction"]
          ["verificationcomplete" "verificationcomplete"]
          ["hahahahahahahahahaha" "hhhhhhhhhhaaaaaaaaaa"]
          ["hahahahahahahahahaha" "aahhhh"]
          
          [(apply str (repeat 10 \s)) (apply str (repeat 10 \~))]
          [(apply str (take 10 (cycle (list \> \_ \= \])))) (apply str (take 10 (cycle (list \q \_))))]
          [(apply str (take 10 (cycle (list \( \))))) (apply str (take 10 (cycle (list \p \p \)))))]
          [(apply str (take 10 (cycle (list \H \a)))) (apply str (take 10 (cycle (list \H \i))))]) 30 0] ; Edge case inputs
   [#(super-anagrams-input 2) 170 0] ; Random inputs. Length 1 strings are covered by hand-coded cases
   [#(super-anagrams-input 1) 0 2000]
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
                 (apply str
                        (interpose \newline
                                   (map #(apply str (interpose \space (rest %)))
                                       (filter first (map #(vector (not= %1 %2) %3 %1 %2)
                                                          in1
                                                          in2
                                                          (range))))))))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn super-anagrams-error-function
  "Returns the error function for the Super Anagrams problem. Takes as
   input Super Anagrams data domains."
  [data-domains]
  (let [[train-cases test-cases] (map super-anagrams-test-cases
                                          (test-and-train-data-from-domains data-domains))]
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
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
                               result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj result))
                           ; Error is Levenshtein distance of printed strings
                           (levenshtein-distance correct-output result))))] ;;NOTE: SEE NOTE IN INTRO
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

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
        (println (format "Test Case  %3d | Error: %d" i error))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best-program :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (super-anagrams-error-function super-anagrams-data-domains)
   :atom-generators super-anagrams-atom-generators
   :max-points 1000
   :max-points-in-initial-program 500
   :evalpush-limit 2000
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
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   ;:max-error 1
   })
