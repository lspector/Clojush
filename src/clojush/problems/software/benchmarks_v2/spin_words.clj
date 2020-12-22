;; spin-words.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.spin-words
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as str]))

(defn word-generator
  "Generates words at a nice distribution for Spin Words
   80% of the time word will have length in range [1, 8].
   20% of the time it will have length in range [1, 16]"
  []
  (let [chars-between #(map char (range (int %1) (inc (int %2))))
        chars (chars-between \a \z)
        word-len (inc (rand-int (if (< (rand) 0.8)
                                  8
                                  16)))]
    (apply str (repeatedly word-len #(rand-nth chars)))))

(defn spin-words-input
  "Makes a Spin Words input of length len."
  [len]
  (let [words (apply str
                     (take len           ; This looks weird because str isn't lazy, so you
                           (apply str    ; need to take len twice here.
                                  (take len
                                        (interpose " " (repeatedly word-generator))))))]
    (if (not= (last words) \space)
      words
      (apply str (butlast words)))))

; Atom generators
(def spin-words-atom-generators
  (concat (list
            5
            \space
            ;;; end constants
            (fn [] (lrand-nth (map char (range 97 122)))) ;Visible character ERC
            (fn [] (spin-words-input (lrand-int 21))) ;String ERC
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def spin-words-data-domains
  [[(list ""
          "a"
          "this is a test"
          "this is another test"
          "hi"
          "cat"
          "walk"
          "jazz"
          "llama"
          "heart"
          "pantry"
          "helpful"
          "disrespectful"
          "stop spinning my words"
          "couple longer words"
          "onelooooooooongworrrrrrrrrrrrrrrrrrrrrrd"
          "word less than five char") 17 0] ;; "Special" inputs covering some base cases
   [(fn [] (spin-words-input (inc (lrand-int 25)))) 183 2000]
   ])

;;Can make Spin Words test data like this:
;(test-and-train-data-from-domains spin-words-data-domains)

; Helper function for error function
(defn spin-words-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (str/join " " (map #(if (>= (count %) 5) (apply str (reverse %)) %) (str/split in #" ")))))
       inputs))

(defn get-spin-words-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map spin-words-test-cases
      (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def spin-words-train-and-test-cases
  (get-spin-words-train-and-test spin-words-data-domains))

(defn make-spin-words-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-spin-words-error-function
    ([individual]
      (the-actual-spin-words-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-spin-words-error-function individual data-cases false))
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
                             result (stack-ref :string 0 final-state)]
                         (when print-outputs
                           (println (format "\n| Correct output: %s\n| Program output: %s" (str correct-output) (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is Levenshtein distance for string
                         (levenshtein-distance correct-output (str result))
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn spin-words-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first spin-words-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second spin-words-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn spin-words-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Spin Words problem report - generation %s\n" generation)(flush)
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
 {:error-function (make-spin-words-error-function-from-cases (first spin-words-train-and-test-cases)
                                                                     (second spin-words-train-and-test-cases))
  :atom-generators spin-words-atom-generators
  :max-points 1600
  :max-genome-size-in-initial-program 200
  :evalpush-limit 1500
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
  :problem-specific-report spin-words-report
  :problem-specific-initial-report spin-words-initial-report
  :report-simplifications 0
  :final-report-simplifications 5000
  :max-error 100000
  })
