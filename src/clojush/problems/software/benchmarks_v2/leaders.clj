;; leaders.clj
;; Peter Kelly (pxkelly@hamilton.edu)
;;
;; Problem Source: https://practice.geeksforgeeks.org/problems/leaders-in-an-array/0

(ns clojush.problems.software.benchmarks-v2.leaders
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

;; Define test cases
(defn leaders-input
  "Makes a leaders input given a length len"
  [len]
  (vec (repeatedly len #(rand-int 1001))))

; Atom generators
(def leaders-atom-generators
  (concat (list
            []
            ;;; end constants
            (fn [] (leaders-input (lrand-int 21))) ;Vector ERC
            ;;; end ERCs
            (tag-instruction-erc [:integer :vector_integer :exec :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :vector_integer :exec :boolean])))

(def leaders-data-domains
  [[(list []
          [0]
          [1000]
          [1000 0]
          [5 5 5 5 5 5 5]
          [10 9 8 7 6 5 4 3 2 1 0]
          [0 1 2 3 4 5 6 7 8 9 10]) 7 0]
   [(fn [] (leaders-input (inc (lrand-int 20)))) 193 2000]
  ])

;;Can make leaders test data like this:
;(test-and-train-data-from-domains leaders-data-domains)

; Helper function for error function
(defn leaders-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (loop [leaders [] elements in]
              (cond
                (= (count elements) 0) leaders
                (= (apply max elements) (first elements)) (recur (conj leaders (first elements)) (rest elements))
                :else (recur leaders (rest elements))))))
       inputs))

(defn make-leaders-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-leaders-error-function
    ([individual]
      (the-actual-leaders-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-leaders-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input correct-output] (case data-cases
                                                                    :train train-cases
                                                                    :test test-cases
                                                                    [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input :input)))
                             result (top-item :vector_integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %2s | Program output: %s" (str correct-output) (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer error at each position in the vectors, with additional penalties for incorrect size vector
                         (if (vector? result)
                           (+' (apply +' (map (fn [cor res]
                                                (abs (- cor res)))
                                              correct-output
                                              result))
                               (*' 1000 (abs (- (count correct-output) (count result))))) ; penalty of 1000 times difference in sizes of vectors
                           1000000) ; penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-leaders-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map leaders-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def leaders-train-and-test-cases
  (get-leaders-train-and-test leaders-data-domains))

(defn leaders-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first leaders-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second leaders-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn leaders-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Leaders problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-leaders-error-function-from-cases (first leaders-train-and-test-cases)
                                                                      (second leaders-train-and-test-cases))
   :atom-generators leaders-atom-generators
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
   :problem-specific-report leaders-report
   :problem-specific-initial-report leaders-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
