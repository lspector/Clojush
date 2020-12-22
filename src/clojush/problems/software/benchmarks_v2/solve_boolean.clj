;; solve_boolean.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.solve-boolean
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def solve-boolean-atom-generators
  (concat (list
            true
            false
            "t"
            "f"
            "&"
            "|"
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :string :char])))

(defn solve-boolean-input
  [terms]
  (loop [in "" terms-left terms]
    (cond
      (= terms-left 0) (apply str in)
      (= terms-left 1) (recur (concat in (rand-nth '("t" "f"))) (dec terms-left))
      :else (recur (concat in (concat (rand-nth '("t" "f")) (rand-nth '("&" "|")))) (dec terms-left)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def solve-boolean-data-domains
  [[(list "t"
          "f"
          "f&f"
          "t&t"
          "f&t"
          "t|f"
          ) 6 0]
   [(fn [] (solve-boolean-input (inc (lrand-int 20)))) 194 2000]
   ])

;;Can make Solve Boolean test data like this:
; (map sort (test-and-train-data-from-domains solve-boolean-data-domains))

(defn bool-solve
  "Helper function to solve test cases"
  [bool]
  (cond
    (= bool "t|t") "t"
    (= bool "t|f") "t"
    (= bool "f|t") "t"
    (= bool "t&t") "t"
    :else "f"))

(defn solve-boolean-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
        (vector in
          (loop [terms in]
            (if (= (count terms) 1) (if (= terms "t") true false)
              (recur (apply str (concat (bool-solve (subs terms 0 3)) (drop 3 terms))))))))
       inputs))

(defn make-solve-boolean-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-solve-boolean-error-function
    ([individual]
      (the-actual-solve-boolean-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-solve-boolean-error-function individual data-cases false))
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
                               result (top-item :boolean final-state)]
                             (when print-outputs
                               (println (format "Correct output: %s | Program output: %s" correct-output (str result))))
                           ; Record the behavior
                           (swap! behavior conj result)
                           ; Error is right or wrong
                           (if (= correct-output result)
                              0
                              1))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-solve-boolean-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map solve-boolean-test-cases
          (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def solve-boolean-train-and-test-cases
  (get-solve-boolean-train-and-test solve-boolean-data-domains))

(defn solve-boolean-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first solve-boolean-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second solve-boolean-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn solve-boolean-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Solve Boolean problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-solve-boolean-error-function-from-cases (first solve-boolean-train-and-test-cases)
                                                             (second solve-boolean-train-and-test-cases))
   :atom-generators solve-boolean-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 1000
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
   :problem-specific-report solve-boolean-report
   :problem-specific-initial-report solve-boolean-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 0
   :max-error 1000000.0
   })
