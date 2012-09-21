;; bioavailability.clj
;;
;; This problem is a symbolic regression problem with 359 fitness cases
;; over 241 variables. Each fitness case is a candidate drug compound,
;; with each variable being a bi-dimensional molecule descriptor of
;; that compound. The data file data/bioavailability.txt
;; contains the data, with the last column being the target variable,
;; in this case human oral bioavailability (%F), a float in the
;; range [0.0, 100.0].
;;
;; The experimental procedure is copied from the paper below. This
;; procedure uses 70% of the fitness cases as a training set, and the
;; remaining 30% as a test set. Each time this file is run, it will
;; select random training (251 fitness cases) and test (108 fitness cases)
;; sets from the fitness cases to use throughout the entire run.
;;
;; See this paper for more information about this problem:
;; Sara Silva and Leonardo Vanneschi. 2009. Operator equalisation,
;; bloat and overfitting: a study on human oral bioavailability
;; prediction. In Proceedings of the 11th Annual conference on
;; Genetic and evolutionary computation (GECCO '09). ACM,
;; New York, NY, USA, 1115-1122. DOI=10.1145/1569901.1570051
;; http://doi.acm.org/10.1145/1569901.1570051 
;;
;; Data available from:
;;  http://personal.disco.unimib.it/Vanneschi/bioavailability.txt
;;
;; Tom Helmuth, thelmuth@cs.umass.edu, 2012

(ns clojush.examples.bioavailability
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.random]
        [local-file]
        ;[clojush.evaluate] ;;remove later
        ;[clojush.individual] ;;remove later
        [clojure.math.numeric-tower])
  (:require [clojure.string :as string]
            [clojure-csv.core :as csv]))

(defn read-data []
  "Reads data from data/bioavailability.txt into a sequence of sequences."
  (let [f (slurp* "src/clojush/examples/data/bioavailability.txt")
        lines (csv/parse-csv f :delimiter \tab)]
    (map #(map (fn [x] (float (read-string x)))
               %)
         lines)))

;(count (read-data))

;(count (first (read-data)))

;(map last (read-data))


(defn define-fitness-cases
  "Returns a map with two keys: train and test. Train maps to a
   subset of 251 random fitness cases (70%), and test maps to the
   remaining 108 fitness cases (30%). These sets are different each
   time this is called."
  []
  (let [fitness-cases-shuffled (shuffle (read-data))
        train-num 251]
    {:train (subvec fitness-cases-shuffled 0 train-num)
     :test (subvec fitness-cases-shuffled train-num)}))

;; Define the fitness cases. Do this once per run, so that train and test
;; subsets stay the same throughout a run.
(def bioavailability-fitness-cases (define-fitness-cases))


;; Helper functions to get specific train and test cases
;(defn train-fitness-case
;  "Returns train fitness case number n for this run."
;  [n]
;  (nth (:train bioavailability-fitness-cases) n))
;
;(defn test-fitness-case
;  "Returns test fitness case number n for this run."
;  [n]
;  (nth (:test bioavailability-fitness-cases) n))


;; I want x0 through x240 to be instructions that, when executed, push
;; the float from that column onto the float stack.
(doseq [[numb symb] (map #(vector % (symbol (str "x" %))) (range 241))]
  (eval `(define-registered ~symb (fn [state#] (push-item (stack-ref :auxiliary ~numb state#) :float state#)))))
  
;(sort (map str (vec @registered-instructions)))


;; This definition of atom-generators makes it so that choosing a terminal
;; has equivalent probability of choosing one of the operators. This is
;; the method used in the paper above.
(def bioavailability-atom-generators
  (list
    (fn [] (lrand-nth (list 'float_div 'float_mult 'float_add 'float_sub)))
    (fn [] (lrand-nth (for [n (range 241)]
                        (symbol (str "x" n)))))
    ))

;; Test random code generation
;(random-code 100 bioavailability-atom-generators)



(defn bioavailability-error-function
  [program]
    (doall
      (for [fitness-case (:train bioavailability-fitness-cases)]
        (let [input (butlast fitness-case)
              output (last fitness-case)
              state (run-push program
                              (assoc (make-push-state)
                                     :auxiliary
                                     input))
              top-float (top-item :float state)]
          (if (number? top-float)
            (abs (- output top-float))
            10000.0)))))

;(run-push '(4 5 integer_add x0 x1 x2 float_add x240)
;          (assoc (make-push-state)
;                 :auxiliary
;                 (butlast (first (:train (define-fitness-cases))))))
;
;(evaluate-individual (make-individual :program (random-code 100 bioavailability-atom-generators))
;                     bioavailability-error-function
;                     (new java.util.Random))
;
;(evaluate-individual (make-individual :program '(x2 x19 float_mult))
;                     bioavailability-error-function
;                     (new java.util.Random))

(pushgp
  :error-function bioavailability-error-function
  :atom-generators bioavailability-atom-generators
  :max-points 500
  :evalpush-limit 500
  :population-size 500
  :max-generations 100
  :mutation-probability 0.1
  :crossover-probability 0.8
  :simplification-probability 0.05
  :tournament-size 6
  :trivial-geography-radius 10
  :node-selection-method :size-tournament
  :node-selection-tournament-size 2
  :report-simplifications 0
  :final-report-simplifications 1000
  )
