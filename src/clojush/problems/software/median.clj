;; median.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: Program Repair Benchmark Paper (add citation later)
;;
;; Program must inputs 3 integers, and return their median.
;;
;; input stack has the 3 integers

(ns clojush.problems.software.median
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def num-io-atom-generators
  (list
    (fn [] (- (lrand-int 201) 100))
    (fn [] (- (* (lrand) 200) 100.0))
    (tag-instruction-erc [:float :integer] 1000)
    (tagged-instruction-erc 1000)
    ;;; end ERCs
    'in1
    'in2
    'print_float
    'print_integer
    ;;; end IO instructions
    'float_rot
    'float_yank
    'float_sin
    'float_frominteger
    'float_cos
    'float_stackdepth
    'float_swap
    'float_div
    'float_shove
    'float_sub
    'float_yankdup
    'float_add
    'float_tan
    'float_mult
    'float_max
    'float_pop
    'float_eq
    'float_min
    'float_dup
    'float_mod
    ;;; end float instructions
    'integer_add
    'integer_swap
    'integer_yank
    'integer_dup
    'integer_yankdup
    'integer_shove
    'integer_mult
    'integer_div
    'integer_max
    'integer_sub
    'integer_mod
    'integer_rot
    'integer_min
    'integer_inc
    'integer_dec
    'integer_fromfloat
    'integer_stackdepth
    ;;; end integer instructions
;    'exec_y
;    'exec_pop
;    'exec_eq
;    'exec_stackdepth
;    'exec_rot
;    'exec_when
;    'exec_do*times
;    'exec_do*count
;    'exec_s
;    'exec_do*range
;    'exec_if
;    'exec_k
;    'exec_yank
;    'exec_yankdup
;    'exec_swap
;    'exec_dup
;    'exec_shove
    ;;; end exec instructions
    ))

;; A list of data domains for the number IO problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def num-io-data-domains
  [[(fn [] (vector (- (* (lrand) 200) 100.0) (- (lrand-int 201) 100))) 25 1000] ;; Each input is a float and an int, both from range [-100,100]
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

;;Can make number-IO test data like this:
;(test-and-train-data-from-domains num-io-data-domains)

; Helper function for error function
(defn num-io-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[float-input int-input] output]."
  [inputs]
  (map #(vector %
                (apply + %))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn num-io-error-function
  "Returns the error function for the number IO problem. Takes as
   input number IO data domains."
  [data-domains]
  (let [[train-cases test-cases] (map num-io-test-cases
                                      (test-and-train-data-from-domains data-domains))]
    (when true ;; Change to false to not print test cases
      (doseq [[i [[in-float in-int] out]] (map vector (range) train-cases)]
        (println (format "Train Case %3d | Float %18.14f | Int %4d | Out %19.14f" i in-float in-int out)))
      (doseq [[i [[in-float in-int] out]] (map vector (range) test-cases)]
        (println (format "Test Case  %3d | Float %18.14f | Int %4d | Out %19.14f" i in-float in-int out)))
    (fn the-actual-wc-error-function
      ([program]
        (the-actual-wc-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-wc-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (flatten
                       (doall
                         (for [[[in-float in-int] out-float] (case data-cases
                                                                          :train train-cases
                                                                          :test test-cases
                                                                          [])]
                           (let [final-state (run-push program
                                                       (->> (make-push-state)
                                                         (push-item in-int :input)
                                                         (push-item in-float :input)
                                                         (push-item "" :output)))
                                 printed-result (stack-ref :output 0 final-state)]
                             (when print-outputs
                               (println (format "Correct output: %-19s | Program output: %-19s" (str out-float) printed-result)))
                             ; Record the behavior
                             (when @global-print-behavioral-diversity
                               (swap! behavior conj printed-result))
                             ; Each test case results in two error values:
                             ;   1. Numeric difference between correct output and the printed
                             ;      output read into a float; if such a conversion fails, the
                             ;      error is a penalty of 1000.
                             ;   2. Levenstein distance between printed output and correct output as strings
                             (vector ((if (= :lexicase @global-parent-selection) #(round (* 1.0E4 %)) identity) ; If we're using lexicase, multiply by 10^4 and round
                                       (try (min 1000.0 (abs (- out-float (Double/parseDouble printed-result))))
                                         (catch Exception e 1000.0)))
                                     (levenshtein-distance printed-result (str out-float)))))))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors))))))

(defn num-io-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Number IO problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 1.0E-4)
      (doseq [[i [num-error lev-dist]] (map vector
                                            (range)
                                            (partition 2 best-test-errors))]
        (println (format "Test Case  %3d | Numeric Error: %19.14f | Levenshtein Distance: %d" i (float num-error) lev-dist))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best-program :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (num-io-error-function num-io-data-domains)
   :atom-generators num-io-atom-generators
   :max-points 200
   :max-points-in-initial-program 100
   :evalpush-limit 200
   :population-size 1000
   :max-generations 200
   :parent-selection :lexicase
   :epigenetic-markers []
   :genetic-operator-probabilities {:alternation 0.3
                                    :uniform-mutation 0.2
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 5
   :uniform-mutation-rate 0.01
   :problem-specific-report num-io-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000
   :error-threshold 1.0E-4
   })
