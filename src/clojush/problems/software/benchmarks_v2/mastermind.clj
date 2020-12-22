;; mastermind.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.mastermind
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def mastermind-atom-generators
  (concat (list
            \B
            \R
            \W
            \Y
            \O
            \G
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :exec :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :exec :string :char :boolean])))

;; Define test cases
(defn mastermind-input
  "Makes a mastermind input."
  []
  (vector (apply str (repeatedly 4 #(rand-nth "BRWYOG"))) (apply str (repeatedly 4 #(rand-nth "BRWYOG")))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def mastermind-data-domains
  [[(list ["RRRR" "RRRR"]
          ["BOYG" "GYOB"]
          ["WYYW" "BBOG"]
          ["GGGB" "BGGG"]
          ["BBBB" "OOOO"]
          ) 5 0]
   [(fn [] (mastermind-input)) 195 2000]
  ])

;;Can make mastermind test data like this:
;(test-and-train-data-from-domains mastermind-data-domains)

(defn remove-matches
  [code guess]
  (loop [check-code code check-guess guess matchless-code "" matchless-guess ""]
    (cond
      (= check-code "") (vector matchless-code matchless-guess)
      (= (first check-code) (first check-guess))
        (recur (subs check-code 1) (subs check-guess 1) matchless-code matchless-guess)
      :else (recur (subs check-code 1) (subs check-guess 1) (str matchless-code (first check-code)) (str matchless-guess (first check-guess))))))

; Helper function for error function
(defn mastermind-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
         (vector [in1 in2]
           (let [[matchless-code matchless-guess] (remove-matches in1 in2)
                 code-frequencies (frequencies matchless-code)
                 guess-frequencies (frequencies matchless-guess)
                 white-pegs (reduce (fn [count [element frequency]] (+ count (min frequency (get guess-frequencies element 0)))) 0 code-frequencies)]
                 (vector white-pegs (- 4 (count matchless-code))))))
       inputs))

(defn make-mastermind-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-mastermind-error-function
    ([individual]
      (the-actual-mastermind-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-mastermind-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                    (doall
                     (for [[[input1 input2] [correct-output1 correct-output2]]
                                                     (case data-cases
                                                     :train train-cases
                                                     :test test-cases
                                                     [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input1 :input)
                                                     (push-item input2 :input)))
                             result1 (stack-ref :integer 0 final-state)
                             result2 (try (stack-ref :integer 1 final-state)
                                          (catch Exception e :no-stack-item))]
                         (when print-outputs
                           (println (format "Correct output: %s %s | Program output: %s %s" (str correct-output1) (str correct-output2)
                                                                                            (str result1) (str result2))))
                         ; Record the behavior
                         (swap! behavior conj result1 result2)
                         ; Error is integer distance
                         (vector
                           (if (number? result1)
                               (abs (- result1 correct-output1)) ;distance from correct integer
                               1000000) ;penalty for no return value
                           (if (number? result2)
                               (abs (- result2 correct-output2)) ;distance from correct integer
                               1000000) ;penalty for no return value
                           )))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-mastermind-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map mastermind-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def mastermind-train-and-test-cases
  (get-mastermind-train-and-test mastermind-data-domains))

(defn mastermind-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first mastermind-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second mastermind-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn mastermind-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Mastermind problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-mastermind-error-function-from-cases (first mastermind-train-and-test-cases)
                                                                  (second mastermind-train-and-test-cases))
   :atom-generators mastermind-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
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
   :problem-specific-report mastermind-report
   :problem-specific-initial-report mastermind-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
