;; levenshtein.clj
;; Lee Spector, lspector@hampshire.edu
;;
;; Given two string inputs, return the Levenshtein distance between them.
;; The input strings will not have tabs or newlines, but may have multiple 
;; spaces in a row. They will have maximum length of 100 characters. 
;;
;; This problem is defined to be solved with a different small set of test
;; cases each generation, so errors will change from generation to generation
;; even for the same program.
;;
;; input stack has the two input strings


(ns clojush.problems.software.levenshtein
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
  (:require [clojure.string :as string]))

(defn random-string
  "Returns a random string of letters, digits, and spaces, of max length n."
  [n]
  (apply str 
         (repeatedly (lrand-int (inc n)) 
                     (fn []
                       (if (< (lrand) 0.2)
                         \space
                         (lrand-nth (map char (range 32 127))))))))

(def levenshtein-atom-generators
  (concat (list
            (fn [] (- 100 (lrand-int 201)))
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            'in1
            'in2)
          (registered-for-stacks [:integer :boolean :string :char :exec])))

(defn generate-levenshtein-cases
  []
  (repeatedly 10 (fn []
                   (let [str1 (random-string 100)
                         str2 (random-string 100)]
                     [[str1 str2] (levenshtein-distance str1 str2)]))))

(def levenshtein-cases (atom (generate-levenshtein-cases)))

(println "Initial test cases:" (pr-str @levenshtein-cases))

(def levenshtein-validation-cases
  (apply concat (repeatedly 10 generate-levenshtein-cases)))

(println "Validation cases:" (pr-str levenshtein-validation-cases))

(defn levenshtein-error
  ([program]
    (levenshtein-error program @levenshtein-cases))
  ([program cases]
    (let [behavior (atom '())
          errors (doall
                   (for [[[str1 str2] distance] cases]
                     (let [state (->> (make-push-state)
                                   (push-item str1 :input)
                                   (push-item str2 :input)
                                   (run-push program))
                           top-int (top-item :integer state)]
                       (when @global-print-behavioral-diversity
                         (swap! behavior conj [top-int]))
                       (if (number? top-int)
                         (#(if (neg? %) (- %) %) (- top-int distance))
                         1000))))]
      (when @global-print-behavioral-diversity
        (swap! population-behaviors conj @behavior))
      errors)))

(defn levenshtein-report
  "Not actually used to report; instead, change test cases."
  [best population generation error-function report-simplifications]
  (let [validation-errors (levenshtein-error (:program best) 
                                             levenshtein-validation-cases)]
    (println "Validation errors:" validation-errors)
    (println "Total validation errors:" (reduce + validation-errors)))
  (let [new-cases (generate-levenshtein-cases)]
    (println "New test cases:" (pr-str new-cases))
    (reset! levenshtein-cases new-cases)))

(def argmap
  {:error-function levenshtein-error
   :atom-generators levenshtein-atom-generators
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
   :problem-specific-report levenshtein-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   :use-single-thread false
   :reuse-errors false
   })
