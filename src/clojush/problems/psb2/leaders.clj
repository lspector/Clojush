;; leaders.clj
;; Peter Kelly (pxkelly@hamilton.edu)
;;
;; Problem inspired by: https://www.codewars.com/kata/5a651865fd56cb55760000e0

(ns clojush.problems.psb2.leaders
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]))

; Define test cases
(defn leaders-input
  "Makes a leaders input given a length len, which is a vector of numbers 0-1000"
  [len]
  (vec (repeatedly len #(rand-int 1001))))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :vector_integer :exec :boolean]) ; stacks
    (list (tag-instruction-erc [:integer :vector_integer :exec :boolean] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list [] ; constants
         (fn [] (leaders-input (lrand-int 21)))) ; vector ERC
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

(def data-domains
  [[(list []
          [0]
          [451]
          [1000 0]
          [0 1000]
          [20 137 20]
          [47 87 43 44]
          [5 5 5 5 5 5 5]
          [10 9 8 7 6 5 4 3 2 1 0]
          [0 1 2 3 4 5 6 7 8 9 10]) 10 0] ; "Special" inputs covering some base cases
   [(fn [] (leaders-input (inc (lrand-int 20)))) 190 2000]])

; Helper function for error function
(defn create-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (loop [leaders []
                        elements in]
                   (cond
                     (empty? elements) leaders
                     (= (apply max elements) (first elements)) (recur (conj leaders (first elements)) (rest elements))
                     :else (recur leaders (rest elements))))))
       inputs))

(defn make-error-function-from-cases
  "Creates and returns the error function based on the train/test cases."
  [train-cases test-cases]
  (fn the-actual-error-function
    ([individual]
     (the-actual-error-function individual :train))
    ([individual data-cases] ; data-cases should be :train or :test
     (the-actual-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (doall
                   (for [[input correct-output] (case data-cases
                                                  :train train-cases
                                                  :test test-cases
                                                  data-cases)]
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
       (if (= data-cases :test)
         (assoc individual :test-errors errors)
         (assoc individual
                :behaviors (reverse @behavior)
                :errors errors))))))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map create-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def train-and-test-cases
  (get-train-and-test data-domains))

(defn initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn custom-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Leaders problem report - generation %s\n" generation) (flush)
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
    )) ; To do validation, could have this function return an altered best individual
       ; with total-error > 0 if it had error of zero on train but not on validation
       ; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-error-function-from-cases (first train-and-test-cases)
                                                           (second train-and-test-cases))
   :training-cases (first train-and-test-cases)
   :atom-generators atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5}
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report custom-report
   :problem-specific-initial-report initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000})