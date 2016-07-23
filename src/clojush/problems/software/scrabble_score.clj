;; scrabble_score.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string of visible characters with length <= 20, return the Scrabble
;; score for that string. Each letter has a corresponding value according to
;; normal Scrabble rules, and non-letter character are worth zero.
;;
;; input stack has the input string

(ns clojush.problems.software.scrabble-score
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

(def scrabble-letter-values
  (let [scrabble-map {\a 1
                      \b 3
                      \c 3
                      \d 2
                      \e 1
                      \f 4
                      \g 2
                      \h 4
                      \i 1
                      \j 8
                      \k 5
                      \l 1
                      \m 3
                      \n 1
                      \o 1
                      \p 3
                      \q 10
                      \r 1
                      \s 1
                      \t 1
                      \u 1
                      \v 4
                      \w 4
                      \x 8
                      \y 4
                      \z 10}
        visible-chars (map char (range 0 127))]
    (vec (for [c visible-chars]
           (get scrabble-map (first (string/lower-case c)) 0)))))

; Atom generators
(def scrabble-score-atom-generators
  (concat (list
            scrabble-letter-values
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :vector_integer :exec])))

;; Define test cases
(defn scrabble-score-input
  "Makes a Scrabble Score input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (lrand-nth (concat (list \newline \tab)
                                          (map char (range 32 127))))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def scrabble-score-data-domains
  [[(map (comp str char) (range 97 123)) 26 0] ;; Lowercase single letters
   [(map (comp str char) (range 65 91)) 0 26] ;; Uppercase single letters
   [(list "", "*", " ", "Q ", "zx", " Dw", "ef", "!!", " F@", "ydp", "4ps"
          "abcdefghijklmnopqrst"
          "ghijklmnopqrstuvwxyz"
          "zxyzxyqQQZXYqqjjawp"
          "h w h j##r##r\n+JJL"
          (apply str (take 13 (cycle (list \i \space \!))))
          (apply str (repeat 20 \Q))
          (apply str (repeat 20 \$))
          (apply str (repeat 20 \w))
          (apply str (take 20 (cycle (list \1 \space))))
          (apply str (take 20 (cycle (list \space \v))))
          (apply str (take 20 (cycle (list \H \a \space))))
          (apply str (take 20 (cycle (list \x \space \y \!))))
          (apply str (take 20 (cycle (list \G \5))))) 24 0] ;; "Special" inputs covering some base cases
   [(fn [] (scrabble-score-input (+ 2 (lrand-int 19)))) 150 974] ;; Random strings with at least 2 characters
   ])

;;Can make Scrabble Score test data like this:
;(test-and-train-data-from-domains scrabble-score-data-domains)

; Helper function for error function
(defn scrabble-score-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply + (map #(nth scrabble-letter-values (int %)) in))))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn scrabble-score-error-function
  "Returns the error function for the Scrabble Score problem. Takes as
   input Scrabble Score data domains."
  [data-domains]
  (let [[train-cases test-cases] (map #(sort-by (comp count first) %)
                                      (map scrabble-score-test-cases
                                           (test-and-train-data-from-domains data-domains)))]
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-scrabble-score-error-function
      ([program]
        (the-actual-scrabble-score-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-scrabble-score-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (doall
                       (for [[input1 correct-output] (case data-cases
                                                                  :train train-cases
                                                                  :test test-cases
                                                                  [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input1 :input)))
                               result (stack-ref :integer 0 final-state)]
                           (when print-outputs
                             (println (format "Correct output: %3d | Program output: %s" correct-output (str result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj result))
                           ; Error is difference of integers
                           (if (number? result)
                             (abs (- result correct-output)) ;distance from correct integer
                             1000) ;penalty for no return value
                           )))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(defn scrabble-score-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Scrabble Score problem report - generation %s\n" generation)(flush)
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
  {:error-function (scrabble-score-error-function scrabble-score-data-domains)
   :atom-generators scrabble-score-atom-generators
   :max-points 4000
   :max-genome-size-in-initial-program 500
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
   :problem-specific-report scrabble-score-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000
   })
