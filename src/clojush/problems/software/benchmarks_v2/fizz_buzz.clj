;; fizz_buzz.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.fizz-buzz
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def fizz-buzz-atom-generators
  (concat (list
            "Fizz"
            "Buzz"
            "FizzBuzz"
            3
            5
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :exec :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :exec :string :boolean])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def fizz-buzz-data-domains
  [[(list 1
          2
          3
          4
          5
          6
          7
          8
          9
          10
          11
          12
          13
          14
          15
          16
          17
          18
          19
          20
          49995
          49998
          49999
          50000
          ) 24 0]
   [(fn [] (inc (lrand-int 1000000))) 176 2000]
  ])

;;Can make Fizz Buzz test data like this:
;(test-and-train-data-from-domains fizz-buzz-data-domains)

; Helper function for error function
(defn fizz-buzz-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
           (cond
             (and (= (mod in 3) 0)
                  (= (mod in 5) 0)) "FizzBuzz"
             (= (mod in 3) 0) "Fizz"
             (= (mod in 5) 0) "Buzz"
             :else (str in))))
       inputs))

(defn make-fizz-buzz-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-fizz-buzz-error-function
    ([individual]
      (the-actual-fizz-buzz-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-fizz-buzz-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input1 correct-output] (case data-cases
                                                     :train train-cases
                                                     :test test-cases
                                                     [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input1 :input)))
                             result (stack-ref :string 0 final-state)]
                         (when print-outputs
                           (println (format "Correct output: %s | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is Levenshtein distance
                         (levenshtein-distance correct-output (str result))
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-fizz-buzz-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map fizz-buzz-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def fizz-buzz-train-and-test-cases
  (get-fizz-buzz-train-and-test fizz-buzz-data-domains))

(defn fizz-buzz-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first fizz-buzz-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second fizz-buzz-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn fizz-buzz-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Fizz Buzz problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-fizz-buzz-error-function-from-cases (first fizz-buzz-train-and-test-cases)
                                                                  (second fizz-buzz-train-and-test-cases))
   :atom-generators fizz-buzz-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 4000
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
   :problem-specific-report fizz-buzz-report
   :problem-specific-initial-report fizz-buzz-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
