;; small_or_large.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given an integer n in the range [-10000, 10000], print "small" if n < 1000
;; and "large" if n >= 2000 (and nothing if 1000 <= n < 2000).
;;
;; input stack has input integer n

(ns clojush.problems.software.small-or-large-c
  (:use clojush.pushgp.pushgp
        clojush.pushgp.genetic-operators
        [clojush pushstate interpreter random util globals simplification translate]
        clojush.instructions.tag
        ;clojush.instructions.environment
        [clojure.math numeric-tower]
        )
  (:require [clojush.problems.software.small-or-large :as sol]))

(defn uniform-segment-addition-to-culture
  "Returns the individual with each segment possibly duplicated. The probability that a
  segment will be duplicated is given by uniform-duplication-rate. The probability that
  segmenting will occur at each gene is given by uniform-segmenting-rate.
  Works with Plushy genomes."
  [ind local-tagspace uniform-segment-culture-rate uniform-segmenting-rate]
  (let [segmented (partition-by (fn [_]
                                  (< (rand)
                                     (random-element-or-identity-if-not-a-collection
                                       uniform-segmenting-rate)))
                                (:genome ind))
        rate (random-element-or-identity-if-not-a-collection uniform-segment-culture-rate)]
    (doseq [seg segmented]
      (if (< (rand) rate)
        (let [x (rand-int 500)]
          (if (not (contains? @local-tagspace key))
            (swap! local-tagspace assoc x (translate-plush-genome-to-push-program {:genome seg} {:max-points @global-max-points}))
            ))
        ))))

(defn merge-tagspaces
  [gts gts-fit lts total-error]
  (let [lt (into (sorted-map) (map #(vector (first %1) (vector (second %1) total-error)) (apply vector @lts)))
        gt (if (empty? @gts)
             lt
             (merge-with vector @gts @gts-fit))
        ]
    (doseq [key (keys lt)]
      (if (get gt key)
        (if (> (second (get gt key)) (second (get lt key)))
          (do
            (swap! gts assoc key (first (get lt key)))
            (swap! gts-fit assoc key (second (get lt key))))
          (do
            (swap! gts assoc key (first (get gt key)))
            (swap! gts-fit assoc key (second (get gt key))))
          )
        (if (> (second (val (closest-association key {:tag gt}))) (second (get lt key)))
          (do
            (swap! gts assoc key (first (get lt key)))
            (swap! gts-fit assoc key (second (get lt key))))
          )
        ))
    ))


; Atom generators
(def small-or-large-atom-generators
  (concat (list
           "small"
           "large"
            ;;; end constants
           (fn [] (- (lrand-int 20001) 10000)) ;Integer ERC [-10000,10000]
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec :string] 500)
            (tagged-instruction-erc 500)
            (untag-instruction-erc 500)
           (registered-for-type "return_")
            ;;; end tag ERCs
           'in1
            ;;; end input instructions
           )
          (registered-for-stacks [:integer :boolean :exec :string :print])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def small-or-large-data-domains
  [[(concat (list -10000 0 980) (range 995 1005) (list 1020 1980)
            (range 1995 2005) (list 2020 10000)) 27 0] ;; "Special" inputs covering most base cases.
   [(concat (range 980 1020) (range 1980 2020)) 0 80] ;; Some cases to test generality.
   [(fn [] (- (lrand-int 20001) 10000)) 73 920] ;; Inputs between -10,000 and 10,000
   ])

;;Can make Small Or Large test data like this:
;(test-and-train-data-from-domains small-or-large-data-domains)

; Helper function for error function
(defn small-or-large-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (cond (< in 1000) "small"
                       (>= in 2000) "large"
                       :else "")))
       inputs))

(defn make-small-or-large-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-small-or-large-error-function
    ([individual]
      (the-actual-small-or-large-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-small-or-large-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            local-tagspace (case data-cases
                             :train (atom @global-common-tagspace)
                             :simplify (atom (:tagspace individual)) ; during simplification and testing, the tagspace should not be changed.
                             :test (atom (:tagspace individual))
                             (atom @global-common-tagspace))
            reuse-metric (atom ())       
            repetition-metric (atom ())            
            cases (case data-cases
                    :train train-cases
                    :simplify train-cases
                    :test test-cases
                    data-cases)
            errors (let [ran nil] ;(rand-nth cases)]
                     (doall
                      (for [[input1 correct-output] cases]
                        (let [final-state (if (= [input1 correct-output] ran)
                                            (run-push (:program (auto-simplify-lite individual
                                                                                    (fn [inp] (sol/make-small-or-large-error-function-from-cases inp nil)) ; error-function per test case
                                                                                    75
                                                                                    (first sol/small-or-large-train-and-test-cases) ; cases
                                                                                    false 100))
                                                      (->> (assoc (make-push-state) :calculate-mod-metrics (= [input1 correct-output] ran))
                                                           (push-item input1 :input)
                                                           (push-item "" :output)))
                                            (run-push (:program individual)
                                                      (->> (assoc (make-push-state) :tag @local-tagspace)
                                                           (push-item input1 :input)
                                                           (push-item "" :output)))
                                            )
                              result (stack-ref :output 0 final-state)
                              _ (if (not= data-cases :test)
                                    (reset! local-tagspace (get final-state :tag)))
                              ]
                          (when print-outputs
                            (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                          
                          (if (= [input1 correct-output] ran)
                            (let [metrics (mod-metrics (:trace final-state) (:trace_id final-state))]
                              (do
                                (swap! reuse-metric conj (first metrics))
                                (swap! repetition-metric conj (last metrics)))))

                        ; Record the behavior
                          (swap! behavior conj result)
                         ; Error is Levenshtein distance of printed strings
                          (levenshtein-distance correct-output result)))))
            _ (if (and (not= data-cases :test) (not= data-cases :simplify)) ;(= data-cases :train)
                (if (let [x (vec errors)
                                       ;_ (prn x)
                          y (first (:history individual))
                                       ;_ (prn y)
                          ]
                      (if (nil? y)
                        true
                        ; (some? (some true? (map #(< %1 %2) x y))))) ; child is better than mom on at least one test case; can be worse on others
                        ;(every? true? (map #(<= %1 %2) x y))
                        true
                        ))
                  (do
                    (uniform-segment-addition-to-culture individual local-tagspace 0.5 0.1)
                    (merge-tagspaces global-common-tagspace global-common-tagspace-fitness local-tagspace (apply + errors))
                    ;(reset! global-common-tagspace @local-tagspace)
                               ;(prn @global-common-tagspace)
                    )))
            ]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual :behaviors @behavior :errors errors :reuse-info @reuse-metric :repetition-info @repetition-metric :tagspace @local-tagspace)
          )))))

(defn get-small-or-large-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map small-or-large-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def small-or-large-train-and-test-cases
  (get-small-or-large-train-and-test small-or-large-data-domains))

(defn small-or-large-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first small-or-large-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second small-or-large-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn small-or-large-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Small Or Large problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-small-or-large-error-function-from-cases (first small-or-large-train-and-test-cases)
                                                                  (second small-or-large-train-and-test-cases))
   :atom-generators small-or-large-atom-generators
   :max-points 800
   :max-genome-size-in-initial-program 100
   :evalpush-limit 300
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1}
   :uniform-addition-and-deletion-rate 0.09
   ;:genetic-operator-probabilities {:alternation 0.2
   ;                                 :uniform-mutation 0.2
   ;                                 :uniform-close-mutation 0.1
   ;                                 [:alternation :uniform-mutation] 0.5
   ;                                 }
   ;:alternation-rate 0.01
   ;:alignment-deviation 5
   ;:uniform-mutation-rate 0.01
   :problem-specific-report small-or-large-report
   :problem-specific-initial-report small-or-large-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   ;:meta-error-categories [:tag-usage]
   :use-single-thread true
   ;:print-history true
   :pop-when-tagging false
   })
