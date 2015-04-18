(ns clojush.evaluate
  (:use [clojush util pushstate random globals individual])
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
;; calculate meta-errors

(defn calculate-meta-errors
  "Calculates one meta-error for each meta-error category provided. Each
   meta-error-category should either be a keyword for a built-in meta category
   or a function that takes an individual and returns a meta error value.
   The built-in meta categories include:
     :size (minimize size of program)
     :compressibility (minimize ammount a program compresses compared to itself)
     :total-error (minimize total error)
     :unsolved-cases (maximize number of cases with zero error)"
  [ind {:keys [meta-error-categories error-threshold]}]
  (let [meta-error-fn (fn [cat]
                        (cond
                          (fn? cat) (cat ind)
                          (= cat :size) (count (:genome ind))
;                          (= cat :compressibility) 555 ;;TMH fix later
                          (= cat :total-error) (:total-error ind)
                          (= cat :unsolved-cases) (count (filter #(> % error-threshold) (:errors ind)))
                          :else (throw (Exception. (str "Unrecognized meta category: " cat)))))]
    (map meta-error-fn meta-error-categories)))

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
    {:keys [reuse-errors print-history total-error-method normalization max-error pass-individual-to-error-function]
     :as argmap}]
    (random/with-rng rand-gen
      (let [p (:program i)
            raw-errors (if (or (not reuse-errors) (nil? (:errors i)) (nil? (:total-error i)))
                         (if pass-individual-to-error-function
                           (error-function i)
                           (error-function p)))
            e (vec (if (and reuse-errors (not (nil? (:errors i))))
                     (:errors i)
                     (do
                       (swap! evaluations-count inc)
                       (normalize-errors raw-errors normalization max-error)
                       )))
            te (if (and reuse-errors (not (nil? (:total-error i))))
                 (:total-error i)
                 (compute-total-error raw-errors))
            ne (if (and reuse-errors (not (nil? (:normalized-error i))))
                 (:normalized-error i)
                 (compute-total-error e))
            we (case total-error-method
                 :sum nil
                 :ifs nil
                 :hah (compute-hah-error e)
                 :rmse (compute-root-mean-square-error e)
                 nil)
            new-ind (assoc i ; Assign errors and history to i
                           :errors e
                           :total-error te
                           :weighted-error we
                           :normalized-error ne
                           :history (if print-history (cons te (:history i)) (:history i))
                           )
            me (calculate-meta-errors new-ind argmap)]
        (assoc new-ind :meta-errors me)))))
