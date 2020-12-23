;; substitution_cipher.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.substitution-cipher
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def substitution-cipher-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :char :string])))

;; Define test cases
(defn substitution-cipher-input
  "Makes Substitution Cipher inputs 1 and 2 of length len-map and input 3 of
  len-code."
  [len-map len-code]
  (let [available-map "abcdefghijklmnopqrstuvwxyz"
        map1 (take len-map (shuffle (apply list available-map)))
        map2 (take len-map (shuffle (apply list available-map)))]
        (vector
          (apply str map1)
          (apply str map2)
          (apply str (repeatedly len-code #(rand-nth map1))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def substitution-cipher-data-domains
  [[(list ["" "" ""]
          ["a" "a" "a"]
          ["j" "h" "j"]
          ["a" "z" "a"]
          ["e" "l" "eeeeeeeeee"]
          ["h" "d" "hhhhhhhhhhhhhhhhhhhh"]
          ["o" "z" "oooooooooooooooooooooooooo"]
          ["abcdefghijklmnopqrstuvwxyz" "zyxwvutsrqponmlkjihgfedcba" "bvafvuqgjkkbeccipwdfqttgzl"]
          ["abcdefghijklmnopqrstuvwxyz" "cdqutzayxshgfenjowrkvmpbil" "thequickbrownfxjmpsvlazydg"]
          ["otghvwmkclidzryxsfqeapnjbu" "alpebhxmnrcyiosvtgzjwuqdfk" "aaabbbccc"]
          ) 10 0] ;; "Special" inputs covering most base cases.
   [(fn [] (substitution-cipher-input (inc (lrand-int 26)) (inc (rand-int 26)))) 190 2000]
   ])

;;Can make substitution-cipher test data like this:
;(test-and-train-data-from-domains substitution-cipher-data-domains)

; Helper function for error function
(defn substitution-cipher-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2 in3]]
         (vector [in1 in2 in3]
           (apply str (map (zipmap in1 in2) (map char in3)))))
       inputs))

(defn make-substitution-cipher-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-substitution-cipher-error-function
    ([individual]
      (the-actual-substitution-cipher-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-substitution-cipher-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                      (doall
                       (for [[[input1 input2 input3] correct-output] (case data-cases
                                                                     :train train-cases
                                                                     :test test-cases
                                                                     [])]
                         (let [final-state (run-push (:program individual)
                                                     (->> (make-push-state)
                                                       (push-item input1 :input)
                                                       (push-item input2 :input)
                                                       (push-item input3 :input)))
                               result (top-item :string final-state)]
                           (when print-outputs
                             (println (format "| Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                           ; Record the behavior
                           (swap! behavior conj result)
                           ; Error is Levenshtein distance
                           (levenshtein-distance correct-output (str result))
                       ))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-substitution-cipher-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map substitution-cipher-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def substitution-cipher-train-and-test-cases
  (get-substitution-cipher-train-and-test substitution-cipher-data-domains))

(defn substitution-cipher-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first substitution-cipher-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second substitution-cipher-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn substitution-cipher-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Substitution Cipher problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-substitution-cipher-error-function-from-cases (first substitution-cipher-train-and-test-cases)
                                                                  (second substitution-cipher-train-and-test-cases))
   :atom-generators substitution-cipher-atom-generators
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
   :problem-specific-report substitution-cipher-report
   :problem-specific-initial-report substitution-cipher-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
