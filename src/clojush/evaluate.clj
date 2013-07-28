(ns clojush.evaluate
  (:use [clojush.util]
        [clojush.pushstate]
        [clojush.random]
        [clojush.globals]
        [clojush.individual])
  (:require [clojure.math.numeric-tower :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluate individuals

(defn compute-total-error
  [errors]
  (reduce +' errors))

(defn compute-root-mean-square-error
  [errors]
  (if @global-use-rmse
    (math/sqrt (/ (apply +' (map #(* % %)
                                 errors))
                  (count errors)))
    nil))

(defn compute-hah-error
  [errors]
  (if @global-use-historically-assessed-hardness
    (reduce +' (doall (map (fn [rate e] (*' (- 1.01 rate) e))
                           @solution-rates
                           errors)))
    nil))

(defn calculate-hah-solution-rates
  [use-historically-assessed-hardness pop-agents error-threshold population-size]
  (when use-historically-assessed-hardness
    (reset! solution-rates
            (let [error-seqs (map :errors (map deref pop-agents))
                  num-cases (count (first error-seqs))]
              (doall (for [i (range num-cases)]
                       (/ (count (filter #(<= % error-threshold)
                                         (map #(nth % i) error-seqs)))
                          population-size)))))
    (printf "\nSolution rates: ")
    (println (doall (map float @solution-rates)))))

(defn evaluate-individual
  "Returns the given individual with errors, total-errors, and hah-errors,
   computing them if necessary."
  [i error-function rand-gen]
  (binding [*thread-local-random-generator* rand-gen]
    (let [p (:program i)
          e (if (and (seq? (:errors i)) @global-reuse-errors)
              (:errors i)
              (error-function p))
          te (if (and (number? (:total-error i)) @global-reuse-errors)
               (:total-error i)
               (keep-number-reasonable (compute-total-error e)))
          he (compute-hah-error e)
          rmse (compute-root-mean-square-error e)]
      (make-individual :program p :errors e :total-error te :hah-error he :rms-error rmse
                       :history (if maintain-histories (cons te (:history i)) (:history i))
                       :ancestors (:ancestors i)
                       :parent (:parent i)))))
