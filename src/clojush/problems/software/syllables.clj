;; syllables.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given a string (max length 20, containing symbols, spaces, digits, and
;; lowercase letters), count the number of occurrences of vowels (a,e,i,o,u,y)
;; in the string and print that number as X in "The number of syllables is X"
;;
;; input stack has the input string

(ns clojush.problems.software.syllables
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

;; Define test cases
(defn syllables-input
  "Makes a Syllables input of length len."
  [len]
  (apply str
         (repeatedly len
                     #(if (< (lrand) 0.2)
                        (lrand-nth "aeiouy")
                        (lrand-nth (map char (concat (range 32 65) (range 91 127))))))))

; Atom generators
(def syllables-atom-generators
  (concat (list
            "The number of syllables is "
            \a
            \e
            \i
            \o
            \u
            \y
            "aeiouy"
            ;;; end constants
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            (fn [] (syllables-input (lrand-int 21))) ;String ERC
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
(def syllables-data-domains
  [[(list "", "a", "v", "4", "o", " ", "aei", "ouy", "chf", "quite", "a r e9j>"
          "you are many yay yea"
          (apply str (repeat 20 \s))
          (apply str (repeat 20 \o))
          (apply str (take 20 (cycle (list \w \i \space))))
          (apply str (take 20 (cycle (list \x \space \y \space))))
          (apply str (take 20 (cycle (list \e \i \o \y))))) 17 0] ;; Hand-chosen edge cases
   [(fn [] (syllables-input (inc (lrand-int 20)))) 83 1000]
   ])

;;Can make syllables test data like this:
;(test-and-train-data-from-domains syllables-data-domains)

; Helper function for error function
(defn syllables-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (str "The number of syllables is " (count (filter #(some #{%} "aeiouy") in)))))
       inputs))

(defn make-syllables-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-syllables-error-function
    ([program]
      (the-actual-syllables-error-function program :train))
    ([program data-cases] ;; data-cases should be :train or :test
                          (the-actual-syllables-error-function program data-cases false))
    ([program data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                     (doall
                       (for [[input correct-output] (case data-cases
                                                      :train train-cases
                                                      :test test-cases
                                                      [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input :input)
                                                       (push-item "" :output)))
                               printed-result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str printed-result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj printed-result))
                           ; Error is Levenshtein distance and, if ends in an integer, distance from correct integer
                           (vector
                             (levenshtein-distance correct-output printed-result)
                             (if-let [num-result (try (Integer/parseInt (last (string/split printed-result #"\s+")))
                                                   (catch Exception e nil))]
                               (abs (- (Integer/parseInt (last (string/split correct-output #"\s+")))
                                       num-result)) ;distance from correct integer
                               1000)
                             )))))]
        (when @global-print-behavioral-diversity
          (swap! population-behaviors conj @behavior))
        errors))))

(defn get-syllables-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map syllables-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def syllables-train-and-test-cases
  (get-syllables-train-and-test syllables-data-domains))

(defn syllables-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first syllables-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second syllables-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn syllables-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Syllables problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best-program :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-syllables-error-function-from-cases (first syllables-train-and-test-cases)
                                                             (second syllables-train-and-test-cases))
   :atom-generators syllables-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 1600
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
   :problem-specific-report syllables-report
   :problem-specific-initial-report syllables-initial-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
