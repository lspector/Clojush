;; cut_vector.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.cut-vector
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

;; Define test cases
(defn cut-vector-input
  "Makes a Cut Vector input vector of length len."
  [len]
  (vec (repeatedly len #(rand-int 10000))))

; Atom generators
(def cut-vector-atom-generators
  (concat (list
            ;;; end constants
            (fn [] (cut-vector-input (inc (lrand-int 20)))) ;Vector ERC
            ;;; end ERCs
            (tag-instruction-erc [:vector_integer :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:vector_integer :integer :boolean :exec])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def cut-vector-data-domains
  [[(list [0] [100] [-100] [1000] [-1000]) 5 0] ;; Length-1 vectors
   [(fn [] (cut-vector-input 1)) 20 250] ;; Random Length-1 vectors
   [(list [2 129]
          [1 -4]
          [999 74]
          [987 995]
          [-788 -812]) 5 0] ;; Length-2 vectors
   [(fn [] (cut-vector-input 2)) 20 250] ;; Random Length-2 vectors
   [(fn [] (cut-vector-input (+ 3 (lrand-int 3)))) 50 500] ;; Random Length-3, -4, and -5 vectors
   [(fn [] (cut-vector-input 20)) 5 50] ;; Random Length-20 vectors
   [(fn [] (cut-vector-input (inc (lrand-int 20)))) 95 950] ;; Random length, random ints
   ])

;;Can make Cut Vector test data like this:
;(test-and-train-data-from-domains cut-vector-data-domains)

; Helper function for error function
(defn cut-vector-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
    (vector in
      (loop [cut 1 minimum (apply max in) min-index 0]
        (cond
          (= (count in) 1) [in []]
          (= (count in) cut) [(subvec in 0 min-index) (subvec in min-index)]
          (= (reduce + (subvec in 0 cut)) (reduce + (subvec in cut))) [(subvec in 0 cut) (subvec in cut)]
          (>= minimum (abs (- (reduce + (subvec in 0 cut)) (reduce + (subvec in cut))))) (recur (inc cut) (abs (- (reduce + (subvec in 0 cut)) (reduce + (subvec in cut)))) cut)
          :else (recur (inc cut) minimum min-index)
          ))))
       inputs))

(defn make-cut-vector-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-cut-vector-error-function
    ([individual]
     (the-actual-cut-vector-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-cut-vector-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors
              (flatten
                (doall
                   (for [[input1 [correct-output1 correct-output2]] (case data-cases
                                                   :train train-cases
                                                   :test test-cases
                                                   [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)))
                           result1 (stack-ref :vector_integer 0 final-state)
                           result2 (try (stack-ref :vector_integer 1 final-state)
                                        (catch Exception e :no-stack-item))]
                       (when print-outputs
                           (println (format "Correct output: %s %s\n| Program output: %s %s\n" (str correct-output1) (str correct-output2) (str result1) (str result2))))
                       ; Record the behavior
                       (swap! behavior conj result1 result2)
                       ; Error is integer error at each position in the vectors, with additional penalties for incorrect size vector
                       (vector
                         (if (vector? result1)
                           (+' (apply +' (map (fn [cor res]
                                                (abs (- cor res)))
                                              correct-output1
                                              result1))
                               (*' 10000 (abs (- (count correct-output1) (count result1))))) ; penalty of 10000 times difference in sizes of vectors
                           1000000000) ; penalty for no return value
                         (if (vector? result2)
                           (+' (apply +' (map (fn [cor res]
                                                (abs (- cor res)))
                                              correct-output2
                                              result2))
                               (*' 10000 (abs (- (count correct-output2) (count result2))))) ; penalty of 10000 times difference in sizes of vectors
                           1000000000) ; penalty for no return value
                       )))))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-cut-vector-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map cut-vector-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def cut-vector-train-and-test-cases
  (get-cut-vector-train-and-test cut-vector-data-domains))

(defn cut-vector-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first cut-vector-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second cut-vector-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn cut-vector-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (error-function best :test)
        best-test-errors (:test-errors best-with-test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Cut Vector problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-cut-vector-error-function-from-cases (first cut-vector-train-and-test-cases)
                                                                  (second cut-vector-train-and-test-cases))
   :atom-generators cut-vector-atom-generators
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
   :problem-specific-report cut-vector-report
   :problem-specific-initial-report cut-vector-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
