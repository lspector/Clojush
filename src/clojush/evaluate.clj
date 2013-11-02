(ns clojush.evaluate
  (:use [clojush.util]
        [clojush.pushstate]
        [clojush.random]
        [clojush.globals]
        [clojush.individual])
  (:require [clojure.math.numeric-tower :as math]
            [clj-random.core :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluate individuals

(defn compute-total-error
  [errors]
  (reduce +' errors))

(defn compute-root-mean-square-error
  [errors]
  (math/sqrt (/ (apply +' (map #(* % %)
                               errors))
                (count errors))))

(defn compute-hah-error
  [errors]
  (reduce +' (doall (map (fn [rate e] (*' (- 1.01 rate) e))
                         @solution-rates
                         errors))))

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
  ([i error-function rand-gen]
    (evaluate-individual i error-function rand-gen
                         {:reuse-errors true
                          :print-history false
                          :use-rmse false
                          :use-historically-assessed-hardness false}))
  ([i error-function rand-gen {:keys [reuse-errors print-history use-rmse
                                      use-historically-assessed-hardness]}]
    (random/with-rng rand-gen
      (let [p (:program i)
            e (vec (if (and (not (nil? (:errors i))) reuse-errors)
                     (:errors i)
                     (error-function p)))
            te (if (and (not (nil? (:total-error i))) reuse-errors)
                 (:total-error i)
                 (compute-total-error e))
            he (if use-historically-assessed-hardness
                 (compute-hah-error e)
                 nil)
            rmse (if use-rmse
                   (compute-root-mean-square-error e)
                   nil)]
        (make-individual :program p :errors e :total-error te :hah-error he :rms-error rmse
                         :history (if print-history (cons te (:history i)) (:history i))
                         :ancestors (:ancestors i)
                         :parent (:parent i))))))
