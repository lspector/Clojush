(ns clojush.evaluate
  (:use [clojush util pushstate random globals individual meta-errors interpreter simplification mod_metrics]
        clojush.pushgp.genetic-operators)
  (:require [clojure.math.numeric-tower :as math]
            [clj-random.core :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate the solution rates (only called from pushgp)

(defn calculate-hah-solution-rates
  [pop-agents {:keys [total-error-method error-threshold population-size]}]
  (when (= total-error-method :hah)
    (reset! solution-rates
            (let [error-seqs (map :errors (map deref pop-agents))
                  num-cases (count (first error-seqs))]
              (doall (for [i (range num-cases)]
                       (/ (count (filter #(<= % error-threshold)
                                         (map #(nth % i) error-seqs)))
                          population-size)))))
    (printf "\nSolution rates: ")
    (println (doall (map float @solution-rates)))))   

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

(defn normalize-errors
  "Normalizes errors to [0,1] if normalize isn't :none."
  [errors normalization max-error]
  (if (= normalization :none)
    errors
    (map (fn [err]
           (case normalization
             :divide-by-max-error (double (if (>= err max-error)
                                            1.0
                                            (/ err max-error)))
             :e-over-e-plus-1 (double (/ err (inc err)))
             (throw (Exception. (str "Unrecognized argument for normalization: "
                                     normalization)))))
         errors)))

(defn evaluate-individual
  "Returns the given individual with errors, total-errors, and weighted-errors,
   computing them if necessary."
  ([i error-function rand-gen]
   (evaluate-individual i error-function rand-gen
                        {:reuse-errors true
                         :print-history false
                         :total-error-method :sum
                         :normalization :none
                         :max-error 1000}))
  ([i error-function rand-gen
    {:keys [reuse-errors print-history total-error-method normalization max-error
            parent-selection]
     :as argmap}]
   (random/with-rng rand-gen
     (let [p (:program i)
           evaluated-i (cond
                         (and reuse-errors (not (nil? (:errors i))))
                         i
                          ;;
                         (= parent-selection :downsampled-lexicase)
                         (error-function i (:sub-training-cases argmap))
                          ;;
                         :else
                         (error-function i))
           raw-errors (:errors evaluated-i)
           e (vec (if (and reuse-errors (not (nil? (:errors i))))
                    (:errors i)
                    (do
                      (swap! evaluations-count inc)
                      (normalize-errors raw-errors normalization max-error))))
           te (if (and reuse-errors (not (nil? (:total-error i))))
                (:total-error i)
                (compute-total-error raw-errors))
           ne (if (and reuse-errors (not (nil? (:normalized-error i))))
                (:normalized-error i)
                (compute-total-error e))
           we (case total-error-method
                :sum nil
                :ifs nil ; calculated later
                :eliteness nil ; calculated later
                :hah (compute-hah-error e)
                :rmse (compute-root-mean-square-error e)
                nil)
           mm (if (:calculate-mod-metrics argmap)
                (let [i (auto-simplify i error-function (:simplification-steps-for-mod-metrics argmap) false 0 argmap)
                      rand-inp (first (rand-nth (:training-cases argmap)))
                      final-state (if (not (sequential? rand-inp))
                                    (run-push (:program i) (push-item rand-inp :input (assoc (make-push-state) :calculate-mod-metrics true))) ; input is a single element (numbers, string , etc)
                                    (if (apply = (map #(count (first %)) (:training-cases argmap))) ; multiple inputs, e.g., three strings
                                      (loop [inp rand-inp
                                             state (assoc (make-push-state) :calculate-mod-metrics true)]
                                        (if (empty? inp)
                                          (run-push (:program i) state)
                                          (recur (rest inp) (push-item (first inp) :input state))))
                                      (run-push (:program i) (push-item rand-inp :input (assoc (make-push-state) :calculate-mod-metrics true))) ; input is a vector of varying length
                                      ))]
                  (reuse (reverse (remove-ids (:trace final-state) :instr) ) (reverse (remove-ids (:trace final-state) :id)))
                  ))
           new-ind (assoc evaluated-i ; Assign errors and history to i
                          :errors e
                          :total-error te
                          :weighted-error we
                          :normalized-error ne
                          :history (if print-history (cons e (:history i)) (:history i))
                          :reuse-info (first mm)
                          :repetition-info (last mm)
                          )]
       new-ind))))