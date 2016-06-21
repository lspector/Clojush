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

(ns clojush.problems.regression.bioavailability
  (:use [clojush.pushgp.pushgp]
        [clojush random util pushstate interpreter]
        [local-file]
        [clojure.math.numeric-tower])
  (:require [clojure.string :as string]
            [clojure-csv.core :as csv]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions

(defn rmse
  "Returns the root of the mean square error for use in error reporting."
  [errors]
  (sqrt (/ (apply + (map #(* % %)
                         errors))
           (count errors))))

(defn read-data []
  "Reads data from data/bioavailability.txt into a sequence of sequences."
  (let [f (slurp* "src/clojush/problems/regression/data/bioavailability.txt")
        lines (csv/parse-csv f :delimiter \tab)]
    (map #(map (fn [x] (float (read-string x)))
               %)
         lines)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define instructions and fitness cases

;; x0 through x240 are instructions that, when executed, push
;; the float from that column onto the float stack.
(doseq [[numb symb] (map #(vector % (symbol (str "x" %))) (range 241))]
  (eval `(define-registered ~symb (fn [state#] (push-item (stack-ref :auxiliary ~numb state#) :float state#)))))

;; Define the fitness cases. Do this once per run, so that train and test
;; subsets stay the same throughout a run.
(def bioavailability-fitness-cases (define-fitness-cases))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main functions to pass to pushgp

;; This definition of atom-generators makes it so that choosing a terminal
;; has equivalent probability of choosing one of the operators. This is
;; the method used in the paper above.
(def bioavailability-atom-generators
  (list
    (fn [] (lrand-nth (list 'float_div 'float_mult 'float_add 'float_sub)))
    (fn [] (lrand-nth (for [n (range 241)]
                        (symbol (str "x" n)))))
    ))

(defn bioavailability-error-function
  "Error function for the bioavailability problem."
  [fitness-set program]
    (doall
      (for [fitness-case (get bioavailability-fitness-cases fitness-set)]
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

(defn bioavailability-report
  "Customize generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (not-lazy (:program best))
        best-test-errors (bioavailability-error-function :test best-program)]
    (printf ";; -*- Bioavailability problem report generation %s" generation)(flush)
    (printf  "\nTest mean: %.4f"
            (float (/ (apply + best-test-errors)
                      (count best-test-errors))))(flush)
    (printf "\nTest RMSE: %.4f" (rmse best-test-errors))(flush)
    (printf "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n")(flush)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main call

;;;; The parameters used in the paper mentioned at the top are:
;:population-size 500
;:max-generations 100
;:mutation-probability 0.09
;:crossover-probability 0.81
;;:replication-rate 0.1
;:tournament-size 10
;;:max-depth 17 ;tree GP param
;;:max-dept-of-mutation-code 6 ;tree GP param

(def argmap
  {:error-function (partial bioavailability-error-function :train)
   :atom-generators bioavailability-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 500
   :evalpush-limit 500
   :population-size 500
   :max-generations 100
   :epigenetic-markers []
   :genetic-operator-probabilities {:reproduction 0.1
                                    :alternation 0.81
                                    :uniform-mutation 0.09}
   :parent-selection :tournament
   :tournament-size 10
   :total-error-method :rmse
   :report-simplifications 0
   :final-report-simplifications 1000
   :problem-specific-report bioavailability-report
   })
