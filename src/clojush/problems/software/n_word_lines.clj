;; n_word_lines.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: Program Repair Benchmark Paper (add citation later)
;;
;; Given an integer 1 <= N <= 10 and a string of at most 100 characters that likely
;; contains spaces and newlines, print the string with exactly N words per line.
;; The last line may have fewer than N words.
;;
;; input stack has the input string and integer

(ns clojush.problems.software.n-word-lines
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

; Atom generators
(def n-word-lines-atom-generators
  (concat (list
            \space
            \newline
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; Define test cases
(defn n-word-lines-input
  "Makes a N-Word Lines input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (let [r (lrand)]
                         (cond
                           (< r 0.15) \space
                           (< r 0.2) \newline
                           :else (lrand-nth (map char (range 32 127)))))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def n-word-lines-data-domains
  [[(list ["" 1]
          ["" 4]
          ["A" 1]
          ["*" 6]
          [" " 7]
          ["\n" 1]
          ["s" 2]
          ["B " 1]
          ["  " 1]
          [" D" 2]
          ["2\n" 1]
          ["ef" 1]
          ["!!" 1]
          [" F " 1]
          ["T L" 1]
          ["4 s" 2]
          ["o\n&" 1]
          ["e\ne" 2]
          ["q  " 1]
          ["\n e" 1]
          ["hi " 4]
          ["q e\n" 1]
          ["  $  " 3]
          ["\n\n\nr\n" 1]
          ["9r 2 33 4" 1]
          ["9 22 3d 4r" 2]
          ["9 2a 3 4 g" 2]
          ["9 2a 3 4 g" 10]
          ["Well well, what is this?\n211 days in a row that you've stopped by to see ME?\nThen, go away!" 3]
          [(apply str (take 76 (cycle (list \i \space \!)))) 4]
          [(apply str (repeat 100 \space)) 6]
          [(apply str (repeat 100 \newline)) 1]
          [(apply str (repeat 100 \s)) 7]
          [(apply str (take 100 (cycle (list \$ \space)))) 1]
          [(apply str (take 100 (cycle (list \1 \space)))) 4]
          [(apply str (take 100 (cycle (list \newline \r)))) 1]
          [(apply str (take 100 (cycle (list \newline \v)))) 10]
          [(apply str (take 100 (cycle (list \d \newline \space)))) 10]
          [(apply str (take 100 (cycle (list \H \a \space)))) 9]
          [(apply str (take 100 (cycle (list \x \space \y \!)))) 5]
          [(apply str (take 100 (cycle (list \K \space \h \newline)))) 1]
          [(apply str (take 100 (cycle (list \G \space \w \newline)))) 8]
          [(apply str (take 100 (cycle (list \space \space \3 \space \space \newline \newline \space \space)))) 7]
          [(apply str (take 100 (cycle (list \> \_ \= \])))) 2]
          [(apply str (take 100 (cycle (list \^ \_ \^ \space)))) 1]) 45 0] ; Edge case inputs
   [(fn [] [(n-word-lines-input (inc (lrand-int 100)))
            (lrand-nth (concat (range 1 11) (range 1 6) (range 1 4)))]) 105 2000] ; Random inputs. For N, [1,3] will have 1/6 chance each, [4,5] will have 1/9 chance each, and [6,10] will have 1/18 chance each
   ])

(defn test-and-train-data-from-domains
  "Takes a list of domains and creates a set of (random) train inputs and a set of test
   inputs based on the domains. Returns [train test]. A program should not
   be considered a solution unless it is perfect on both the train and test
   cases."
  [domains]
  (apply mapv concat (map (fn [[input-set n-train n-test]]
                            (if (fn? input-set)
                              (vector (repeatedly n-train input-set)
                                      (repeatedly n-test input-set))
                              (vector (if (>= n-train (count input-set))
                                        input-set
                                        (take n-train (shuffle input-set)))
                                      (if (>= n-test (count input-set))
                                        input-set
                                        (take n-test (shuffle input-set))))))
                          domains)))

;;Can make N-Word Lines test data like this:
;(test-and-train-data-from-domains n-word-lines-data-domains)

; Helper function for error function
(defn n-word-lines-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in-str in-int]]
         (vector [in-str in-int]
                 (apply str
                        (flatten (interpose (list \newline)
                                           (map #(interpose \space %)
                                                (partition-all in-int (string/split (string/trim in-str) #"\s+"))))))))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn n-word-lines-error-function
  "Returns the error function for the N-Word Lines problem. Takes as
   input N-Word Lines data domains."
  [data-domains]
  (let [[train-cases test-cases] (map #(sort-by (comp count first first) %)
                                      (map n-word-lines-test-cases
                                           (test-and-train-data-from-domains data-domains)))]
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-n-word-lines-error-function
      ([program]
        (the-actual-n-word-lines-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-n-word-lines-error-function program data-cases false))
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
                           (levenshtein-distance correct-output result))))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(defn n-word-lines-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- N-Word Lines problem report - generation %s\n" generation)(flush)
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
  {:error-function (n-word-lines-error-function n-word-lines-data-domains)
   :atom-generators n-word-lines-atom-generators
   :max-points 800
   :max-points-in-initial-program 400
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
   :problem-specific-report n-word-lines-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   ;:max-error 1
   })
