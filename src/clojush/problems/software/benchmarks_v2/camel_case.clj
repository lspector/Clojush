;; camel_case.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem Source: https://www.codewars.com/kata/517abf86da9663f1d2000003

(ns clojush.problems.software.benchmarks-v2.camel-case
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as str]))

;; Define test cases
(defn camel-case-input
  "Makes a Camel Case input of length len."
  ; Code found from https://stackoverflow.com/questions/27053726/how-to-generate-random-password-with-the-fixed-length-in-clojure
  [len]
  (let [chars-between #(map char (range (int %1) (inc (int %2))))
        chars (concat (chars-between \a \z)
                      " "
                      "-")
        word (take len (repeatedly #(rand-nth chars)))]
          (reduce str word)))

; Atom generators
(def camel-case-atom-generators
  (concat (list
            \-
            \space
            ;;; end constants
            (fn [] (lrand-nth (map char (range 97 122)))) ;Visible character ERC
            (fn [] (camel-case-input (lrand-int 21))) ;String ERC
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def camel-case-data-domains
  [[(list ""
          "oneword"
          "two-words"
          "two-words and-a-space"
          "all separate words"
          "all-one-word-with-th-dash"
          "saaaaaaaaaaaaaaaaaaaaaame") 7 0] ;; "Special" inputs covering some base cases
   [(fn [] (camel-case-input (inc (lrand-int 20)))) 193 2000]
   ])

;;Can make Camel Case test data like this:
;(test-and-train-data-from-domains camel-case-data-domains)

; Helper function for error function
(defn camel-case-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (if (or (= (str in) "") (every? #{\-} (str in))) ""
                (let [full-string (str/join (map str/capitalize (str/split (str in) #"-")))]
                     (apply str (str/lower-case (first full-string)) (drop 1 full-string))))))
       inputs))

(defn get-camel-case-train-and-test
  "Returns the train and test cases."
  [data-domains]
       (map camel-case-test-cases
            (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def camel-case-train-and-test-cases
  (get-camel-case-train-and-test camel-case-data-domains))

(defn make-camel-case-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-camel-case-error-function
    ([individual]
      (the-actual-camel-case-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-camel-case-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input correct-output] (case data-cases
                                                    :train train-cases
                                                    :test test-cases
                                                    [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input :input)
                                                     (push-item "" :output)))
                             result (stack-ref :output 0 final-state)]
                         (when print-outputs
                           (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is Levenshtein distance for printed string
                         (levenshtein-distance correct-output result)
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn camel-case-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first camel-case-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second camel-case-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn camel-case-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Camel Case problem report - generation %s\n" generation)(flush)
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
 {:error-function (make-camel-case-error-function-from-cases (first camel-case-train-and-test-cases)
                                                                     (second camel-case-train-and-test-cases))
  :atom-generators camel-case-atom-generators
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
  :problem-specific-report camel-case-report
  :problem-specific-initial-report camel-case-initial-report
  :report-simplifications 0
  :final-report-simplifications 5000
  :max-error 1000000
  })
