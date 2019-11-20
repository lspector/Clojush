;; compare_string_lengths.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given three strings in1, in2, and in3, return true if
;; lenth(in1) < length(in2) < length(in3), and false otherwise.
;;
;; input stack has 3 input strings

(ns clojush.problems.software.compare-string-lengths-c
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def csl-atom-generators
  (concat (list
           (fn [] (lrand-nth (list true false))) ;Boolean ERC
            ;;; end ERCs
           ;(tag-instruction-erc [:integer :boolean :string :exec] 1000)
           ;(tagged-instruction-erc 1000)
           ;(untag-instruction-erc 1000)
            ;;; end tag ERCs
           'in1
           'in2
           'in3
            ;;; end input instructions
           )
          (registered-for-stacks [:integer :boolean :string :exec])))


;; Define test cases
(defn csl-input
  "Makes a Compare String Lengths input string of length len."
  [len]
  (apply str
         (repeatedly len
                     #(lrand-nth (concat [\newline \tab]
                                         (map char (range 32 127)))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def csl-data-domains
  [[(list ["" "" ""]) 1 0] ;; All empty strings
   [(permutations ["" "a" "bc"]) 6 0] ;; Permutations of three small strings
   [(apply concat (repeatedly 2 #(permutations ["" "" (csl-input (inc (lrand-int 49)))]))) 6 0] ;; Cases with 2 empties and a non-empty
   [(apply concat (repeatedly 3 #(permutations (conj (repeat 2 (csl-input (inc (lrand-int 49)))) (csl-input (inc (lrand-int 49))))))) 9 0] ;; Cases with 2 strings repeated
   [(fn [] (repeat 3 (csl-input (lrand-int 50)))) 3 100] ;; Cases where all are the same
   [(fn [] (sort-by count (repeatedly 3 #(csl-input (lrand-int 50))))) 25 200] ;; Cases forced to be in order (as long as two aren't same size randomly, will be true)
   [(fn [] (repeatedly 3 #(csl-input (lrand-int 50)))) 50 700] ;; Cases in random order
   ])

;;Can make Compare String Lengths test data like this:
;(test-and-train-data-from-domains csl-data-domains)

; Helper function for error function
(defn csl-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector %
                (apply < (map count %)))
       inputs))

(defn make-compare-string-lengths-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-csl-error-function
    ([individual]
      (the-actual-csl-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-csl-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            ;local-tagspace (case data-cases
            ;                 :train (if @global-use-lineage-tagspaces
            ;                          (atom (:tagspace individual))
            ;                          (atom @global-common-tagspace))
            ;                 :simplify (atom (:tagspace individual))  ; during simplification, the tagspace should not be changed.
            ;                 :test (atom (:tagspace individual))
            ;                 [])
            ; _ (prn "Before:")
            ; _ (prn @local-tagspace)
            reuse-metric (atom ())
            repetition-metric (atom ())
            cases (case data-cases
                    :train train-cases
                    :simplify train-cases
                    :test test-cases
                    [])
            errors (let [ran (if (= data-cases :train)
                               (rand-nth cases)
                               nil)]
                     (doall
                      (for [[[input1 input2 input3] correct-output] cases]
                        (let [final-state (run-push (:program individual)
                                                    (->> (assoc (make-push-state) :calculate-mod-metrics (= [[input1 input2 input3] correct-output] ran))
                                                   ;(assoc (make-push-state) :tag @local-tagspace)
                                                         (push-item input3 :input)
                                                         (push-item input2 :input)
                                                         (push-item input1 :input)))
                              result (top-item :boolean final-state)
                           ; _ (if (= data-cases :train)
                           ;     (reset! local-tagspace (get final-state :tag)))
                              ]
                          (when print-outputs
                            (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
                          (if (= [[input1 input2 input3] correct-output] ran)
                            (let [metrics (mod-metrics (:trace final-state) (:trace_id final-state))]
                              (do
                                (swap! reuse-metric conj (first metrics))
                                (swap! repetition-metric conj (last metrics)))))
                         ; Record the behavior
                          (swap! behavior conj result)
                         ; Error is boolean error
                          (if (= result correct-output)
                            0
                            1)))))
           ; _ (if (and (= data-cases :train) (not @global-use-lineage-tagspaces))
           ;     (if (let [x (vec errors)
           ;                           ; _ (prn x)
           ;               y (first (:history individual))
           ;                            ;_ (prn y)
           ;               ]
           ;           (if (nil? y)
           ;             true
           ;           ;(some? (some true? (map #(< %1 %2) x y))))) ; child is better than mom on at least one test case; can be worse on others
           ;             (every? true? (map #(<= %1 %2) x y))))
           ;       (do
           ;         (reset! global-common-tagspace @local-tagspace)
           ;                      ;(prn @global-common-tagspace)
           ;         )))
            ]
        (if (or (= data-cases :train) (= data-cases :simplify))
          (assoc individual :behaviors @behavior :errors errors :reuse-info @reuse-metric :repetition-info @repetition-metric); :tagspace @local-tagspace)
          (assoc individual :test-errors errors))))))
  
(defn get-compare-string-lengths-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map csl-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def compare-string-lengths-train-and-test-cases
  (get-compare-string-lengths-train-and-test csl-data-domains))

(defn compare-string-lengths-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first compare-string-lengths-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second compare-string-lengths-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn csl-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Compare String Lengths problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-compare-string-lengths-error-function-from-cases (first compare-string-lengths-train-and-test-cases)
                                                                          (second compare-string-lengths-train-and-test-cases))
   :atom-generators csl-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 600
   :population-size 1000
   :max-generations 300
   :parent-selection :fitness-proportionate
   ;:genetic-operator-probabilities {:uniform-addition-and-deletion 1}
   ;:uniform-addition-and-deletion-rate 0.09
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report csl-report
   :problem-specific-initial-report compare-string-lengths-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1
   ;:use-single-thread true
   ;:print-history true
   ;:use-lineage-tagspaces true
   ;:pop-when-tagging false
   ;:tag-enrichment-types [:integer :boolean :string :exec]
   ;:tag-enrichment 50
   })
