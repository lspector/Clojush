(ns clojush.evaluate
  (:use [clojush util pushstate random globals individual]
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
;; calculate meta-errors

(defn calculate-meta-errors
  "Calculates one meta-error for each meta-error category provided. Each
   meta-error-category should either be a keyword for a built-in meta category
   or a function that takes an individual and an argmap and returns a meta error value.
   The built-in meta categories include:
     :size (minimize size of program)
     :compressibility (minimize ammount a program compresses compared to itself)
     :total-error (minimize total error)
     :unsolved-cases (maximize number of cases with zero error)
     :rand (a random floating-point value)
     :rand-bit (randomly 0 or 1)
     :age (minimize genealogical age of program)"
  [ind {:keys [meta-error-categories error-threshold improvement-discount] :as argmap}]
  (let [meta-error-fn (fn [cat]
                        (cond
                          (fn? cat) (cat ind argmap)
                          (= cat :size) (count (:genome ind))
                          ;(= cat :compressibility) 555 ;;TMH fix later
                          (= cat :total-error) (:total-error ind)
                          (= cat :unsolved-cases) (count (filter #(> % error-threshold) 
                                                                 (:errors ind)))
                          (= cat :rand) (lrand)
                          (= cat :rand-bit) (lrand-nth [0 1])
                          (= cat :age) (:age ind)
                          (= cat :novelty) :novelty ; Keyword will be replaced later,
                          ;                         ; needs entire population to compute novelty
                          ;
                          (= cat :gens-since-total-error-change)
                          (if (not (:print-history argmap))
                            (throw 
                              (Exception. 
                                ":print-history must be true for :gens-since-total-error-change"))
                            (let [hist (mapv (partial reduce +) (:history ind))]
                              (if (or (empty? hist)
                                      (apply = hist))
                                1000000
                                (count (take-while #(= % (first hist)) (rest hist))))))
                          ;
                          (= cat :gens-since-total-error-improvement)
                          (if (not (:print-history argmap))
                            (throw 
                              (Exception. 
                                ":print-history must be true for :gens-since-total-error-improvement"))
                            (let [diffs (mapv (fn [[a b]] (- a b)) 
                                              (partition 2 1 (mapv (partial reduce +) 
                                                                   (:history ind))))]
                              (if (or (empty? diffs)
                                      (not (some neg? diffs)))
                                1000000
                                (count (take-while #(>= % 0) diffs)))))
                          ;
                          (= cat :total-error-improvement-ratio)
                          (if (not (:print-history argmap))
                            (throw 
                              (Exception. 
                                ":print-history must be true for :total-error-improvement-ratio"))
                            (let [diffs (mapv (fn [[a b]] (- a b)) 
                                              (partition 2 1 (mapv (partial reduce +) 
                                                                   (:history ind))))]
                              (if (empty? diffs)
                                1000000
                                (- 1 (/ (count (filter neg? diffs))
                                        (count diffs))))))
                          ;
                          (= cat :total-error-new-best-ratio)
                          (if (not (:print-history argmap))
                            (throw 
                              (Exception. 
                                ":print-history must be true for :total-error-new-best-ratio"))
                            (let [hist (mapv (partial reduce +) (:history ind))]
                              (if (empty? (rest hist))
                                1000000
                                (loop [remaining hist
                                       new-best-count 0]
                                  (if (empty? (rest remaining))
                                    (- 1 (/ new-best-count (dec (count hist))))
                                    (recur (rest remaining)
                                           (+ new-best-count
                                              (if (every? #(> % (first remaining))
                                                          (rest remaining))
                                                1
                                                0))))))))
                          ;
                          (= cat :discounted-total-error-new-best-ratio)
                          (if (not (:print-history argmap))
                            (throw 
                              (Exception. 
                                ":print-history must be true for :discounted-total-error-new-best-ratio"))
                            (let [hist (mapv (partial reduce +) (:history ind))]
                              (if (empty? (rest hist))
                                1000000
                                (loop [remaining hist
                                       new-best-count 0
                                       scale 1
                                       max-total 0]
                                  (if (empty? (rest remaining))
                                    (- 1.0 (/ new-best-count max-total))
                                    (recur (rest remaining)
                                           (+ new-best-count
                                              (if (every? #(> % (first remaining))
                                                          (rest remaining))
                                                (/ 1 scale)
                                                0))
                                           (* 2.0 scale)
                                           (+ max-total (/ 1 scale))))))))
                          ;
                          (= cat :discounted-total-error-improvement-ratio)
                          (if (not (:print-history argmap))
                            (throw 
                              (Exception. 
                                ":print-history must be true, :discounted-total-error-improvement-ratio"))
                            (if (empty? (rest (:history ind)))
                              1000000
                              (let [diffs (mapv (fn [[a b]] (- a b))
                                                (partition 2 1 (mapv (partial reduce +) (:history ind))))
                                    improvements (mapv #(if (neg? %) 1.0 0.0) 
                                                       diffs)
                                    persistence 0.5
                                    weights (take (count diffs) 
                                                  (iterate (partial * persistence) 1))]
                                (- 1 (/ (reduce + (mapv * improvements weights))
                                        (reduce + weights))))))
                          ;
                          (= cat :discounted-case-improvements)
                          (if (not (:print-history argmap))
                            (throw
                              (Exception.
                                ":print-history must be true for :discounted-case-improvements"))
                            (if (empty? (rest (:history ind)))
                              (vec (repeat (count (:errors ind)) 1000000))
                              (vec (for [case-history (apply map list (:history ind))]
                                     (if (zero? (first case-history)) 
                                       ;; note only zero is solved
                                       ;; error-threshold applies to total so can't be used here
                                       0 ;; solved, improvement doesn't matter
                                       (let [improvements (mapv (fn [[newer-error older-error]]
                                                                  (let [imp (- older-error newer-error)]
                                                                    (if (> imp 0)
                                                                      1.0
                                                                      0.0)))
                                                                (partition 2 1 case-history))
                                             weights (iterate (partial * (- 1 improvement-discount)) 0.5)
                                             sum (reduce + (mapv * improvements weights))]
                                         (if (<= sum 0)
                                           1.0E100
                                           ;(* (first case-history) (/ 1.0 sum))
                                           ; arbitrating error ties with improvement 
                                           ;(+ (first case-history) (- 1.0 sum))
                                           ; (mostly) arbitrating improvement ties with error
                                           ;(+ (first case-history) (* 1000 (/ 1.0 sum)))
                                           ;(- 1.0 sum)
                                           (* (first case-history) (- 1.0 sum))
                                           )))))))
                          (= cat :reproductive-infidelity)
                          (let [g (:genome ind)]
                            (- 1.0
                               (sequence-similarity
                                 g
                                 (produce-child-genome-by-autoconstruction g g argmap))))
                          ;
                          (= cat :reproductive-fidelity)
                          (let [g (:genome ind)]
                            (sequence-similarity
                              g
                              (produce-child-genome-by-autoconstruction g g argmap)))
                          ;
                          (= cat :reproductive-inconsistency)
                          (let [g (:genome ind)]
                            (- 1.0
                               (sequence-similarity
                                 (produce-child-genome-by-autoconstruction g g argmap)
                                 (produce-child-genome-by-autoconstruction g g argmap))))
                          ;
                          (= cat :reproductive-consistency)
                          (let [g (:genome ind)]
                            (sequence-similarity
                              (produce-child-genome-by-autoconstruction g g argmap)
                              (produce-child-genome-by-autoconstruction g g argmap)))
                          ;
                          (= cat :similarity-to-most-similar-parent)
                          (if (and (:parent1-genome ind) (:parent2-genome ind))
                            (max (sequence-similarity (:genome ind) (:parent1-genome ind))
                                 (sequence-similarity (:genome ind) (:parent2-genome ind)))
                            1.0)
                          ;
                          (= cat :reproductive-convergence)
                          (if (and (:parent1-genome ind) (:parent2-genome ind))
                            (let [g (:genome ind)
                                  child1-genome (produce-child-genome-by-autoconstruction g g argmap)
                                  child2-genome (produce-child-genome-by-autoconstruction g g argmap)]
                              (max (sequence-similarity g child1-genome)
                                   (sequence-similarity g child2-genome)
                                   (sequence-similarity child1-genome child2-genome)))
                            1.0)
                          :else (throw (Exception. (str "Unrecognized meta category: " cat)))))]
    (vec (flatten (mapv meta-error-fn meta-error-categories)))))

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
    {:keys [reuse-errors print-history total-error-method normalization max-error]
     :as argmap}]
    (random/with-rng rand-gen
      (let [p (:program i)
            evaluated-i (if (or (not reuse-errors)
                                (nil? (:errors i)))
                         (error-function i)
                         i)
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
                 :ifs nil
                 :hah (compute-hah-error e)
                 :rmse (compute-root-mean-square-error e)
                 nil)
            new-ind (assoc evaluated-i ; Assign errors and history to i
                           :errors e
                           :total-error te
                           :weighted-error we
                           :normalized-error ne
                           :history (if print-history (cons e (:history i)) (:history i)))
            me (calculate-meta-errors new-ind argmap)]
        (assoc new-ind :meta-errors me)))))
