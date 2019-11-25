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
(defn replace-space-with-newline-input
  "Makes a Replace Space With Newline input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (if (< (lrand) 0.2)
                         \space
                         (lrand-nth (map char (range 32 127))))))))

; Atom generators
(def replace-space-with-newline-atom-generators
  (concat (list
            \space
            \newline
            ;;; end constants
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            (fn [] (replace-space-with-newline-input (lrand-int 21))) ;String ERC
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
(def replace-space-with-newline-data-domains
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
   [(fn [] (replace-space-with-newline-input (+ 2 (lrand-int 19)))) 70 1000]
   ])

;;Can make Replace Space With Newline test data like this:
;(test-and-train-data-from-domains replace-space-with-newline-data-domains)

; Helper function for error function
(defn replace-space-with-newline-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 [(string/replace in \space \newline)
                  (count (filter #(not= \space %) in))]))
       inputs))

(defn get-replace-space-with-newline-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map replace-space-with-newline-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def replace-space-with-newline-train-and-test-cases
  (get-replace-space-with-newline-train-and-test replace-space-with-newline-data-domains))

(defn replace-space-with-newline-evaluate-program-for-behaviors
  "Evaluates the program on the given list of cases.
   Returns the behaviors, a list of the outputs of the program on the inputs."
  [program cases]
  (flatten
   (doall
    (for [[input output] cases]
      (let [final-state (run-push program
                                  (->> (make-push-state)
                                       (push-item input :input)
                                       (push-item "" :output)))
            printed-result (stack-ref :output 0 final-state)
            int-result (stack-ref :integer 0 final-state)]
        (vector printed-result int-result))))))

(defn replace-space-with-newline-errors-from-behaviors
  "Takes a list of behaviors across the list of cases and finds the error
   for each of those behaviors, returning an error vector."
  [behaviors cases]
  (let [behavior-pairs (partition 2 behaviors)
        output-pairs (map second cases)]
    (flatten
     (map (fn [[printed-result int-result] [correct-printed-output correct-int]]
            (vector
             (levenshtein-distance correct-printed-output printed-result)
             (if (number? int-result)
               (abs (- int-result correct-int)) ;distance from correct integer
               1000)                  ;penalty for no return value
             ))
          behavior-pairs
          output-pairs))))

(defn replace-space-with-newline-error-function
  "The error function for Replace Space With Newline. Takes an individual as input,
   and returns that individual with :errors and :behaviors set."
  ([individual]
   (replace-space-with-newline-error-function individual :train))
  ([individual data-cases] ;; data-cases should be :train or :test
   (let [cases (case data-cases
                 :train (first replace-space-with-newline-train-and-test-cases)
                 :test (second replace-space-with-newline-train-and-test-cases)
                 data-cases)
         behaviors (replace-space-with-newline-evaluate-program-for-behaviors (:program individual)
                                                                              cases)
         errors (replace-space-with-newline-errors-from-behaviors behaviors cases)]
     (if (= data-cases :test)
       (assoc individual :test-errors errors)
       (assoc individual :behaviors behaviors :errors errors)))))

(defn replace-space-with-newline-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first replace-space-with-newline-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second replace-space-with-newline-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn replace-space-with-newline-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (error-function best :test)
        best-test-errors (:test-errors best-with-test)
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
    (doseq [[[correct-output correct-int] [printed-result int-result]]
            (map vector
                 (map second (first replace-space-with-newline-train-and-test-cases))
                 (partition 2 (:behaviors best)))]
      (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str printed-result)))
      (println (format "| Correct integer: %2d | Program integer: %s" correct-int (str int-result))))
    (println ";;******************************")
    ;; return best individual with tests errors added so that those are recorded
    best-with-test))
       ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function replace-space-with-newline-error-function
   :training-cases (first replace-space-with-newline-train-and-test-cases)
   :sub-training-cases '()
   :atom-generators replace-space-with-newline-atom-generators
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
   :problem-specific-report replace-space-with-newline-report
   :problem-specific-initial-report replace-space-with-newline-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
