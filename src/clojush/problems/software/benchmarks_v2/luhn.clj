;; luhn.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.luhn
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def luhn-atom-generators
  (concat (list
            2
            9
            10
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec :vector_integer] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :vector_integer])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def luhn-data-domains
  [[(list
     [0 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3]
     [9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9]
     [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
     [5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5]) 4 0]
   [(fn [] (vec (repeatedly 16 #(rand-int 10)))) 196 2000]
   ])

;;Can make Luhn test data like this:
;(test-and-train-data-from-domains luhn-data-domains)

; Helper function to multiply digit by 2 and subtract 9 (if necessary)
(defn luhn-formula
  [num]
  (if (>= (* num 2) 10)
    (- (* num 2) 9)
    (* num 2)))

; Some code from here: https://stackoverflow.com/questions/36105612/map-a-function-on-every-two-elements-of-a-list
(defn luhn-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
           (reduce + (map #(% %2) (cycle [luhn-formula identity]) in))))
       inputs))

(defn make-luhn-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-luhn-error-function
    ([individual]
      (the-actual-luhn-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-luhn-error-function individual data-cases false))
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
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %s | Program output: %s" correct-output result)))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is difference of integers
                         (if (number? result)
                           (abs (- result correct-output)) ;distance from correct integer
                           100000) ;penalty for no return value
                           )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-luhn-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map luhn-test-cases
      (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def luhn-train-and-test-cases
  (get-luhn-train-and-test luhn-data-domains))

(defn luhn-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first luhn-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second luhn-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn luhn-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Luhn problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-luhn-error-function-from-cases (first luhn-train-and-test-cases)
                                                                   (second luhn-train-and-test-cases))
   :atom-generators luhn-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
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
   :problem-specific-report luhn-report
   :problem-specific-initial-report luhn-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
