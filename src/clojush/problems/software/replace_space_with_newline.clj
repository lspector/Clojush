;; replace_space_with_newline.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string input, print the string, replacing spaces with newlines.
;; The input string will not have tabs or newlines, but may have multiple spaces
;; in a row. It will have maximum length of 20 characters. Also, the program
;; should return the integer count of the non-whitespace characters.
;;
;; input stack has the input string

(ns clojush.problems.software.replace-space-with-newline
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

;; Define test cases
(defn replace-space-input
  "Makes a Replace Space With Newline input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (if (< (lrand) 0.2)
                         \space
                         (lrand-nth (map char (range 32 127))))))))

; Atom generators
(def replace-space-atom-generators
  (concat (list
            \space
            \newline
            ;;; end constants
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            (fn [] (replace-space-input (lrand-int 21))) ;String ERC
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def replace-space-data-domains
  [[(list "", "A", "*", " ", "s", "B ", "  ", " D", "ef", "!!", " F ", "T L", "4ps", "q  ", "   ", "  e", "hi ",
          "  $  ", "      9",
          (apply str (take 13 (cycle (list \i \space \!))))
          (apply str (repeat 20 \8))
          (apply str (repeat 20 \space))
          (apply str (repeat 20 \s))
          (apply str (take 20 (cycle (list \1 \space))))
          (apply str (take 20 (cycle (list \space \v))))
          (apply str (take 20 (cycle (list \H \a \space))))
          (apply str (take 20 (cycle (list \x \space \y \!))))
          (apply str (take 20 (cycle (list \G \5))))
          (apply str (take 20 (cycle (list \> \_ \= \]))))
          (apply str (take 20 (cycle (list \^ \_ \^ \space))))) 30 0] ;; "Special" inputs covering some base cases
   [(fn [] (replace-space-input (+ 2 (lrand-int 19)))) 70 1000]
   ])

;;Can make Replace Space With Newline test data like this:
;(test-and-train-data-from-domains replace-space-data-domains)

; Helper function for error function
(defn replace-space-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 [(string/replace in \space \newline)
                  (count (filter #(not= \space %) in))]))
       inputs))

(defn make-replace-space-with-newline-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-replace-space-error-function
    ([program]
      (the-actual-replace-space-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-replace-space-error-function program data-cases false))
    ([program data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                     (doall
                       (for [[input [correct-output correct-int]] (case data-cases
                                                                    :train train-cases
                                                                    :test test-cases
                                                                    [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input :input)
                                                       (push-item "" :output)))
                               printed-result (stack-ref :output 0 final-state)
                               int-result (stack-ref :integer 0 final-state)]
                           (when print-outputs
                             (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str printed-result)))
                             (println (format "| Correct integer: %2d | Program integer: %s" correct-int (str int-result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj [printed-result int-result]))
                           ; Error is Levenshtein distance for printed string and
                           ; integer distance for returned integer
                           (vector
                             (levenshtein-distance correct-output printed-result)
                             (if (number? int-result)
                               (abs (- int-result correct-int)) ;distance from correct integer
                               1000) ;penalty for no return value
                             )))))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-replace-space-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map replace-space-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def replace-space-train-and-test-cases
  (get-replace-space-train-and-test replace-space-data-domains))

(defn replace-space-with-newline-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first replace-space-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second replace-space-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn replace-space-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Replace Space With Newline problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-replace-space-with-newline-error-function-from-cases (first replace-space-train-and-test-cases)
                                                                              (second replace-space-train-and-test-cases))
   :atom-generators replace-space-atom-generators
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
   :problem-specific-report replace-space-report
   :problem-specific-initial-report replace-space-with-newline-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
