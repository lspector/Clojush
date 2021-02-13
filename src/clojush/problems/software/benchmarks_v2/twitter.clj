;; twitter.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.benchmarks-v2.twitter
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def twitter-atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :exec :string :char])
    (list (tag-instruction-erc [:integer :boolean :exec :string :char] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list 0
         140
         "Too many characters"
         "You didn't type anything"
         "Your tweet has "
         " characters") ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

;; Define test cases
(defn twitter-input
  "Makes a Twitter input string of length len."
  [len]
  (apply str (map char (repeatedly len #(+ (rand-int 94) 33)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def twitter-data-domains
  [[(list ""
          "1"
          "max length tweet that just contains letters and spaces even SOME CAPITAL LETTERS just to MAKE it INTERESTING now repeeeeeeeeeEEEEEEEeeeat it"
          "40172875*&(&(%^^*!@&#()!@&^(*$787031264123984721-43214876*%^#!(@^$_!@^%#$(!#@%$(01234~~``)"
          "Tooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooolong1"
          "(@)&#N)&#@!M#&17t8023n217830123bn6 BN23780BC3879N01nc3n473N962n9768062BC3718N396b21v8365n9072B638705b097B6*&b%&%b(*B5*&%b7%(*vb&V8%v&(85V80%0857(%v97%(*&%v87c%&*c ()0*^c%08v^mN098)vf%9P8V6TfB97b99870)"
          ) 6 0] ;; "Special" inputs covering base cases
    [(fn [] (twitter-input (lrand-int 201))) 194 2000]])

;;Can make Twitter test data like this:
;(test-and-train-data-from-domains twitter-data-domains)

; Helper function for error function
(defn twitter-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
           (cond
             (= (count in) 0) "You didn't type anything"
             (> (count in) 140) "Too many characters"
             :else (format "Your tweet has %d characters" (count in)))))
       inputs))

(defn make-twitter-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-twitter-error-function
    ([individual]
      (the-actual-twitter-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-twitter-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                    (for [[input1 correct-output] (case data-cases
                                                    :train train-cases
                                                    :test test-cases
                                                    data-cases)]
                      (let [final-state (run-push (:program individual)
                                                  (->> (make-push-state)
                                                       (push-item input1 :input)))
                            result (top-item :string final-state)]
                        (when print-outputs
                          (println (format "| Correct output: %s\n| Program output: %s\n" (str correct-output) (str result))))
                         ; Record the behavior
                        (swap! behavior conj result)
                         ; Error is Levenshtein distance
                        (if (string? result)
                          (levenshtein-distance correct-output (str result))
                          10000) ; penalty for no return value
                        )))]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual
                 :behaviors (reverse @behavior)
                 :errors errors))))))

(defn get-twitter-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map twitter-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def twitter-train-and-test-cases
  (get-twitter-train-and-test twitter-data-domains))

(defn twitter-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first twitter-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second twitter-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn twitter-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Twitter problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-twitter-error-function-from-cases (first twitter-train-and-test-cases)
                                                           (second twitter-train-and-test-cases))
   :training-cases (first twitter-train-and-test-cases)
   :atom-generators twitter-atom-generators
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
   :problem-specific-report twitter-report
   :problem-specific-initial-report twitter-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 10000
   })
