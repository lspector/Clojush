;; wallis_pi.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; John Wallis gave an infinite product that converges to pi/4 as the following:
;;      (2/3)*(4/3)*(4/5)*(6/5)*(6/7)*(8/7)*(8/9)*(10/9)*(10/11)*...
;; Given an integer input 1 <= N <= 200, compute an approximation of this
;; product out to N terms. Results are rounded to 5 decimal places.
;;
;; input stack has integer N

(ns clojush.problems.software.wallis-pi
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def wallis-pi-atom-generators
  (concat (list
            ;;; end constants
            (fn [] (- (lrand-int 1001) 500)) ;Integer ERC [-500,500]
            (fn [] (- (lrand-int 21) 10)) ;Integer ERC [-10,10]
            (fn [] (- (* (lrand) 1000.0) 500.0)) ;Float ERC [-500.0,500.0)
            (fn [] (lrand)) ;Float ERC [0,1)
            ;;; end ERCs
            (tag-instruction-erc [:integer :float :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :float :boolean :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def wallis-pi-data-domains
  [[(concat (range 1 13) '(198 199 200)) 15 0] ; Small and large cases
   [(shuffle (range 13 198)) 135 50] ; Random cases [13,197]
   ])

;;Can make Wallis Pi test data like this:
(map sort (test-and-train-data-from-domains wallis-pi-data-domains))

; Helper function for error function
(defn wallis-pi-approximation
  "The Wallis pi approximation to n terms."
  [n]
  (round-to-n-decimal-places (apply *' (map #(apply / %)
                                            (take n (iterate (fn [[x1 x2]]
                                                               (if (> x1 x2)
                                                                 [x1 (+' 2 x2)]
                                                                 [(+' 2 x1) x2]))
                                                             [2 3]))))
                             5))

(defn wallis-pi-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (wallis-pi-approximation in)))
       inputs))

; Define error function. For now, each run uses different random inputs
(defn wallis-pi-error-function
  "Returns the error function for the Wallis Pi problem. Takes as
   input Wallis Pi data domains."
  [data-domains]
  (let [[train-cases test-cases] (map sort (map wallis-pi-test-cases
                                                (test-and-train-data-from-domains data-domains)))]
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-wallis-pi-error-function
      ([program]
        (the-actual-wallis-pi-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-wallis-pi-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (flatten
                       (doall
                         (for [[input1 correct-output] (case data-cases
                                                                    :train train-cases
                                                                    :test test-cases
                                                                    [])]
                           (let [final-state (run-push program
                                                       (->> (make-push-state)
                                                         (push-item input1 :input)))
                                 result (round-to-n-decimal-places
                                          (stack-ref :float 0 final-state)
                                          5)]
                             (when print-outputs
                               (println (format "Correct output: %.5f | Program output: %.5f" correct-output result)))
                             ; Record the behavior
                             (when @global-print-behavioral-diversity
                               (swap! behavior conj result))
                             ; Outputs rounded to 5 decimal places
                             (vector
                               ; Error 1: float absolute error
                               (if (number? result)
                                 (float (abs (- result correct-output))) ;distance from correct integer
                                 1000000.0) ;penalty for no return value
                               ; Error 2: Levenshtein distance of strings
                               (levenshtein-distance (str correct-output) (str result))
                               )))))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(defn wallis-pi-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Wallis Pi problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 0.001)
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
  {:error-function (wallis-pi-error-function wallis-pi-data-domains)
   :atom-generators wallis-pi-atom-generators
   :max-points 600
   :max-points-in-initial-program 300
   :evalpush-limit 8000
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
   :uniform-mutation-constant-tweak-rate 0.9
   :problem-specific-report wallis-pi-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 0.001
   :max-error 1000000.0
   })

;;;;;;;;;;
;; Below here is for testing a hand-written solution.

;(reset! global-evalpush-limit 8000)
;
;(reset! global-max-points 600)
;
;(defn test-program-on-training
;  [program print-outputs]
;  ((wallis-pi-error-function wallis-pi-data-domains) program :train print-outputs))
;
;; This program works. It uses more than 4000 eval-push steps for input 200, and
;; less than 5000. So, I changed the GP to use at most 8000 steps to give room
;; for larger programs.
;(def tom-program
;  '(
;     in1 1.0
;     exec_do*count
;     (
;       integer_dup integer_dup
;       2 integer_mod 0 integer_eq ;; now (even? in1) on boolean stack
;       exec_if
;       (2 integer_add float_frominteger 3 integer_add float_frominteger)
;       (3 integer_add float_frominteger 2 integer_add float_frominteger)
;       float_div float_mult
;       )
;     ))
;
;(test-program-on-training tom-program false)
