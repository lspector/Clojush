(ns clojush.pushgp.evaluate
  (:use [clojush.util]
        [clojush.pushstate]
        [clojush.random]
        [clojush.globals]
        [clojush.pushgp.individual]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluate individuals

(defn compute-total-error
  [errors]
  (reduce +' errors))

(defn compute-hah-error
  [errors]
  (if @global-use-historically-assessed-hardness
    (reduce +' (doall (map (fn [rate e] (*' (- 1.01 rate) e))
                           @solution-rates
                           errors)))
    nil))

(defn calculate-hah-solution-rates
  [use-historically-assessed-hardness use-lexicase-selection use-fast-lexicase-selection
   pop-agents error-threshold population-size]
  (when (and use-historically-assessed-hardness
             (not (or use-lexicase-selection use-fast-lexicase-selection)))
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
          he (compute-hah-error e)]
      (make-individual :program p :errors e :total-error te :hah-error he
                       :history (if maintain-histories (cons te (:history i)) (:history i))
                       :ancestors (:ancestors i)))))
