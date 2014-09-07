;; compare_string_lengths.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given three strings in1, in2, and in3, return true if
;; lenth(in1) < length(in2) < length(in3), and false otherwise.
;;
;; input stack has 3 input strings

(ns clojush.problems.software.compare-string-lengths
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def csl-atom-generators
  (concat (list
            (fn [] (lrand-nth (list true false))) ;Boolean
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; Define test cases
(defn csl-input
  "Makes a Compare String Lengths input of length len."
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
(def csl-data-domains
  [[(list "", "A", "\t", "\n", "B\n", "\n\n",
          (apply str (repeat 50 \newline))
          (apply str (repeat 50 \space))
          (apply str (repeat 50 \s))
          (apply str (take 50 (cycle (list \C \D \newline))))
          (apply str (take 50 (cycle (list \x \newline \y \space))))
          (apply str (take 50 (cycle (list \space \newline))))) 12 0] ;; "Special" inputs covering some base cases
   [(fn [] (csl-input (inc (lrand-int 50)))) 88 1000]
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

;;Can make Compare String Lengths test data like this:
;(test-and-train-data-from-domains csl-data-domains)

; Helper function for error function
(defn csl-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (format "Check sum is %c"
                        (char (+ (mod (apply + (map int %)) 64)
                                 (int \space)))))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn csl-error-function
  "Returns the error function for the csl problem. Takes as
   input Compare String Lengths data domains."
  [data-domains]
  (let [[train-cases test-cases] (map csl-test-cases
                                      (test-and-train-data-from-domains data-domains))]
    (when false ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-csl-error-function
      ([program]
        (the-actual-csl-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-csl-error-function program data-cases false))
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
          errors)))))

(defn csl-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Compare String Lengths problem report - generation %s\n" generation)(flush)
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
  {:error-function (csl-error-function csl-data-domains)
   :atom-generators csl-atom-generators
   :max-points 800
   :max-points-in-initial-program 400
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
   :problem-specific-report csl-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   ;:max-error 1
   })
