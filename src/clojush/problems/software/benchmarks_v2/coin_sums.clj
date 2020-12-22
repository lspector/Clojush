;; coin_sums.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.coin-sums
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; (define-registered
;  output_integer1
;  ^{:stack-types [:integer]}
;  (fn [state]
;    (if (empty? (:integer state))
;      state
;      (let [top-int (top-item :integer state)]
;        (->> (pop-item :integer state)
;             (stack-assoc top-int :output 0))))))
;
; (define-registered
;  output_integer2
;  ^{:stack-types [:integer]}
;  (fn [state]
;    (if (empty? (:integer state))
;      state
;      (let [top-int (top-item :integer state)]
;        (->> (pop-item :integer state)
;             (stack-assoc top-int :output 1))))))
;
; (define-registered
;  output_integer3
;  ^{:stack-types [:integer]}
;  (fn [state]
;    (if (empty? (:integer state))
;      state
;      (let [top-int (top-item :integer state)]
;        (->> (pop-item :integer state)
;             (stack-assoc top-int :output 2))))))
;
; (define-registered
;  output_integer4
;  ^{:stack-types [:integer]}
;  (fn [state]
;    (if (empty? (:integer state))
;      state
;      (let [top-int (top-item :integer state)]
;        (->> (pop-item :integer state)
;             (stack-assoc top-int :output 3))))))

; Atom generators
(def coin-sums-atom-generators
  (concat (list
            1
            5
            10
            25
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def coin-sums-data-domains
  [[(list 1
          5
          10
          25 ; Basic inputs
          41
          109 ; Interesting inputs
          10000 ; Max input
          ) 7 0]
   [(fn [] (inc (rand-int 10000))) 193 2000]])

;;Can make Coin Sums test data like this:
; (map sort (test-and-train-data-from-domains coin-sums-data-domains))

(defn coin-sums-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input [output1 output2 output3 output4]]."
  [inputs]
  (map (fn [in]
    (vector in
      (loop [money-left in pennies 0 nickles 0 dimes 0 quarters 0]
        (cond
          (= money-left 0) (vector pennies nickles dimes quarters)
          (>= money-left 25) (recur (- money-left 25) pennies nickles dimes (inc quarters))
          (>= money-left 10) (recur (- money-left 10) pennies nickles (inc dimes) quarters)
          (>= money-left 5) (recur (- money-left 5) pennies (inc nickles) dimes quarters)
          :else (recur (- money-left 1) (inc pennies) nickles dimes quarters)))))
    inputs))

(defn make-coin-sums-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-coin-sums-error-function
    ([individual]
      (the-actual-coin-sums-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-coin-sums-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors
              (flatten
                  (doall
                       (for [[input [correct-pennies correct-nickles correct-dimes correct-quarters]]
                                                              (case data-cases
                                                               :train train-cases
                                                               :test test-cases
                                                               [])]
                         (let [final-state (run-push (:program individual)
                                                     (->> (make-push-state)
                                                       ; (push-item :no-output :output)
                                                       ; (push-item :no-output :output)
                                                       ; (push-item :no-output :output)
                                                       ; (push-item :no-output :output)
                                                       (push-item input :input)))
                               result-pennies (stack-ref :integer 0 final-state)
                               result-nickles (try (stack-ref :integer 1 final-state)
                                            (catch Exception e :no-stack-item))
                               result-dimes (try (stack-ref :integer 2 final-state)
                                            (catch Exception e :no-stack-item))
                               result-quarters (try (stack-ref :integer 3 final-state)
                                            (catch Exception e :no-stack-item))]
                             (when print-outputs
                               (println (format "Correct output: %s %s %s %s \n| Program output: %s %s %s %s"
                                                (str correct-pennies) (str correct-nickles) (str correct-dimes) (str correct-quarters)
                                                (str result-pennies) (str result-nickles) (str result-dimes) (str result-quarters))))
                           ; Record the behavior
                           (swap! behavior conj result-pennies result-nickles result-dimes result-quarters)
                           ; Error is integer difference for each integer
                           (vector
                             (if (number? result-pennies)
                                 (abs (- result-pennies correct-pennies)) ;distance from correct integer
                                 100000) ;penalty for no return value
                             (if (number? result-nickles)
                                 (abs (- result-nickles correct-nickles)) ;distance from correct integer
                                 100000) ;penalty for no return value
                             (if (number? result-dimes)
                                 (abs (- result-dimes correct-dimes)) ;distance from correct integer
                                 100000) ;penalty for no return value
                             (if (number? result-quarters)
                                 (abs (- result-quarters correct-quarters)) ;distance from correct integer
                                 100000) ;penalty for no return value
                             )))))] ; penalty for no return value
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-coin-sums-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map coin-sums-test-cases
          (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def coin-sums-train-and-test-cases
  (get-coin-sums-train-and-test coin-sums-data-domains))

(defn coin-sums-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first coin-sums-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second coin-sums-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn coin-sums-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Coin Sums problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 0.001)
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
  {:error-function (make-coin-sums-error-function-from-cases (first coin-sums-train-and-test-cases)
                                                             (second coin-sums-train-and-test-cases))
   :atom-generators coin-sums-atom-generators
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
   :problem-specific-report coin-sums-report
   :problem-specific-initial-report coin-sums-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
