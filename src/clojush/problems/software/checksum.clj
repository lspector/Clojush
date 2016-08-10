;; checksum.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given a string (max length 50), compute the integer values of the characters
;; in the string, sum them, take the sum modulo 64, add the value of the \space 
;; character, and then convert that integer back into its corresponding character
;; (the checksum). Program must print "Check sum is X", where X is replaced by
;; the correct checksum.
;;
;; input stack has the input string

(ns clojush.problems.software.checksum
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def checksum-atom-generators
  (concat (list
            "Check sum is "
            \space
            64
            ;;; end constants
            (fn [] (- (lrand-int 257) 128)) ;Integer ERC [-128,128]
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; Define test cases
(defn checksum-input
  "Makes a checksum input of length len."
  [len]
  (apply str
         (repeatedly len
                     #(lrand-nth (concat [\newline \tab]
                                         (map char (range 32 127)))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def checksum-data-domains
  [[(list "", "\t", "\n", "B\n", "\n\n",
          (apply str (repeat 50 \newline))
          (apply str (repeat 50 \space))
          (apply str (repeat 50 \s))
          (apply str (take 50 (cycle (list \C \D \newline))))
          (apply str (take 50 (cycle (list \x \newline \y \space))))
          (apply str (take 50 (cycle (list \space \newline))))) 11 0] ;; "Special" inputs covering some base cases
   [(map str (map char (range 32 127))) 95 0] ; All visible characters once
   [(fn [] (checksum-input 2)) 55 500] ; Random length-2 inputs
   [(fn [] (checksum-input 3)) 50 500] ; Random length-3 inputs
   [(fn [] (checksum-input (+ 2 (lrand-int 49)))) 89 1000] ; Random >= 2 length inputs
   ])

;;Can make checksum test data like this:
;(test-and-train-data-from-domains checksum-data-domains)

; Helper function for error function
(defn checksum-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (format "Check sum is %c"
                        (char (+ (mod (apply + (map int %)) 64)
                                 (int \space)))))
       inputs))

(defn make-checksum-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-checksum-error-function
    ([program]
      (the-actual-checksum-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-checksum-error-function program data-cases false))
    ([program data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                     (doall
                       (for [[input correct-output] (case data-cases
                                                      :train train-cases
                                                      :test test-cases
                                                      [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input :input)
                                                       (push-item "" :output)))
                               printed-result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "Correct output: %-19s | Program output: %-19s" correct-output printed-result)))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj printed-result))
                           ; Error is Levenshtein distance and, if correct format, distance from correct character
                           (vector
                             (levenshtein-distance correct-output printed-result)
                             (if (not (empty? printed-result))
                               (abs (- (int (last correct-output)) (int (last printed-result)))) ;distance from correct last character
                               1000) ;penalty for wrong format
                             )))))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-checksum-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map checksum-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def checksum-train-and-test-cases
  (get-checksum-train-and-test checksum-data-domains))

(defn checksum-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first checksum-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second checksum-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn checksum-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Checksum problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-checksum-error-function-from-cases (first checksum-train-and-test-cases)
                                                            (second checksum-train-and-test-cases))
   :atom-generators checksum-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 400
   :evalpush-limit 1500
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
   :problem-specific-report checksum-report
   :problem-specific-initial-report checksum-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000
   })
