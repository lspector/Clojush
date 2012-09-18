;; bioavailability.clj
;;
;; This problem is a symbolic regression problem with 359 test cases
;; over 241 variables. Each test case is a candidate drug compound,
;; with each variable being a bi-dimensional molecule descriptor of
;; that compound. The data file data/bioavailability.txt
;; contains the data, with the last column being the target variable,
;; in this case human oral bioavailability (%F), a float in the
;; range [0.0, 100.0].
;;
;; The experimental procedure is copied from the paper below. This
;; procedure uses 70% of the test cases as a training set, and the
;; remaining 30% as a test set. Each time this file is run, it will
;; select random training (251 test cases) and test (108 test cases)
;; sets from the test cases to use throughout the entire run.
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
        [local-file])
  (:require [clojure.string :as string]
            [clojure-csv.core :as csv]))

(defn read-data []
  (let [f (slurp* "src/clojush/examples/data/bioavailability.txt")
        lines (csv/parse-csv f :delimiter \tab)]
    (map #(map (fn [x] (float (read-string x)))
               %)
         lines)))

;(count (read-data))

;(count (first (read-data)))

;(map last (read-data))

;(def test-cases-shuffled
;  (shuffle (read-data)))


(defn define-test-cases
  []
  (let [test-cases-shuffled (shuffle (read-data))
        train-num 251]
    {:train (subvec test-cases-shuffled 0 train-num)
     :test (subvec test-cases-shuffled train-num)}))


(count (:test (define-test-cases)))





