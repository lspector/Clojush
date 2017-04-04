(ns clojush.pushgp.parent-selection
  (:use [clojush random globals util])
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection utilities

(defn retain-one-individual-per-error-vector
  "Retains one random individual to represent each error vector."
  [pop]
  (map lrand-nth (vals (group-by #(:errors %) pop))))

(defn possibly-remove-individuals-with-empty-genomes
  "When :autoconstuctive is truthy, and at least one individual in pop has a non-empty
  genome, remove those with empty genomes."
  [pop {:keys [autoconstructive]}]
  (if autoconstructive
    (let [with-non-empty-genomes (filter #(not (empty? (:genome %))) pop)]
      (if (not (empty? with-non-empty-genomes))
        with-non-empty-genomes
        pop))
    pop))

(defn age-mediate
  "If age-mediated-parent-selection is falsy, returns pop. Otherwise, 
  age-mediated-parent-selection should be a vector of [pmin pmax] with pmin and pmax both 
  being between 0 and 1 (inclusive) with pmin + pmax <= 1.0. Then, with probability pmin,
  returns individuals in pop with age @min-age; with probability pmax, returns all of pop;
  with probability (- 1.0 pmin pmax), selects an age cutoff uniformly from those present
  in the population and returns individuals with the cutoff age or lower."
  [pop {:keys [age-mediated-parent-selection]}]
  (if (not age-mediated-parent-selection)
    pop
    (let [rand-val (lrand)
          non-empties (filter #(not (empty? (:genome %))) pop)
          candidates (if (empty? non-empties) pop non-empties)
          age-limit (if (<= rand-val (first age-mediated-parent-selection))
                      @min-age
                      (if (<= rand-val (apply + age-mediated-parent-selection))
                        @max-age
                        (lrand-nth (distinct (map :age candidates)))))]
      (filter (fn [ind] (<= (:age ind) age-limit))
              candidates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tournament selection

(defn tournament-selection
  "Returns an individual that does the best out of a tournament."
  [pop {:keys [tournament-size total-error-method] :as argmap}]
  (let [subpop (age-mediate pop argmap)
        tournament-set (doall
                         (for [_ (range tournament-size)]
                           (lrand-nth subpop)))
        err-fn (case total-error-method
                 :sum :total-error
                 (:hah :rmse :ifs) :weighted-error
                 (throw (Exception. (str "Unrecognized argument for total-error-method: "
                                         total-error-method))))]
    (reduce (fn [i1 i2] (if (< (err-fn i1) (err-fn i2)) i1 i2))
            tournament-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexicase selection

(defn lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order."
  [pop argmap]
  (loop [survivors (retain-one-individual-per-error-vector 
                     (possibly-remove-individuals-with-empty-genomes
                       (age-mediate pop argmap) 
                       argmap))
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; epsilon lexicase selection

(defn mad
  "returns median absolute deviation (MAD)"
  [x]
  (let [; Get median of x
        x-median (median x)
        ; calculate absolute deviation from median
        dev (map #(Math/abs (float (- % x-median)))
                 x)]
    (median dev)))

(defn calculate-epsilons-for-epsilon-lexicase
  "Calculates the epsilon values for epsilon lexicase selection. Only runs once
   per generation. "
  [pop-agents {:keys [epsilon-lexicase-epsilon]}]
  (when (not epsilon-lexicase-epsilon)
    (let [pop (map deref pop-agents)
          test-case-errors (apply map list (map :errors pop))
          meta-case-errors (apply map list (map :meta-errors pop))
          all-errors (concat test-case-errors meta-case-errors)
          epsilons (map mad all-errors)]
      (println "Epsilons for epsilon lexicase:" epsilons)
      (reset! epsilons-for-epsilon-lexicase epsilons))))

(defn epsilon-lexicase-selection
  "Returns an individual that does within epsilon of the best on the fitness cases when 
  considered one at a time in random order."
  [pop {:keys [epsilon-lexicase-epsilon] :as argmap}]
  (loop [survivors (retain-one-individual-per-error-vector 
                     (possibly-remove-individuals-with-empty-genomes
                       (age-mediate pop argmap) 
                       argmap))
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [; If epsilon-lexicase-epsilon is set in the argmap, use it for epsilon.
            ; Otherwise, use automatic epsilon selections, which are calculated once per generation.
             epsilon (if epsilon-lexicase-epsilon
                       epsilon-lexicase-epsilon
                       (nth @epsilons-for-epsilon-lexicase (first cases)))
             min-err-for-case (apply min (map #(nth % (first cases))
                                              (map #(:errors %) survivors)))]
        (recur (filter #(<= (nth (:errors %)
                                 (first cases))
                            (+ min-err-for-case
                               epsilon))
                       survivors)
               (rest cases))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elitegroup lexicase selection

(defn build-elitegroups
  "Builds a sequence that partitions the cases into sub-sequences, with cases 
   grouped when they produce the same set of elite individuals in the population. 
   In addition, if group A produces population subset PS(A), and group B 
   produces population subset PS(B), and PS(A) is a proper subset of PS(B), then 
   group B is discarded. "
  [pop-agents]
  (println "Building case elitegroups...")
  (let [pop (retain-one-individual-per-error-vector (map deref pop-agents))
        cases (range (count (:errors (first pop))))
        elites (map (fn [c]
                      (let [min-error-for-case 
                            (apply min (map #(nth % c) (map :errors pop)))]
                        (filter #(== (nth (:errors %) c) min-error-for-case)
                                pop)))
                    cases)
        all-elitegroups (vals (group-by #(nth elites %) cases))
        pruned-elitegroups (filter (fn [eg]
                                     (let [e (set (nth elites (first eg)))]
                                       (not-any?
                                         (fn [eg2]
                                           (let [e2 (set (nth elites (first eg2)))]
                                             (and (not= e e2)
                                                  (set/subset? e2 e))))
                                         all-elitegroups)))
                                   all-elitegroups)]
    (reset! elitegroups pruned-elitegroups)
    (println (count @elitegroups) "elitegroups:" @elitegroups)))

(defn elitegroup-lexicase-selection
  "Returns an individual produced by elitegroup lexicase selection."
  [pop argmap]
  (loop [survivors (retain-one-individual-per-error-vector 
                     (possibly-remove-individuals-with-empty-genomes
                       (age-mediate pop argmap) 
                       argmap))
         cases (lshuffle (map lrand-nth @elitegroups))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implicit fitness sharing

(defn assign-ifs-error-to-individual
  "Takes an individual and calculates and assigns its IFS based on the summed
   error across each test case."
  [ind summed-reward-on-test-cases]
  (let [ifs-reward (apply +' (map #(if (zero? %2) 1.0 (/ %1 %2))
                                  (map #(- 1.0 %) (:errors ind))
                                  summed-reward-on-test-cases))
        ifs-er (cond
                 (< 1e20 ifs-reward) 0.0
                 (zero? ifs-reward) 1e20
                 (< 1e20 (/ 1.0 ifs-reward)) 1e20
                 :else (/ 1.0 ifs-reward))]
    (assoc ind :weighted-error ifs-er)))

(defn calculate-implicit-fitness-sharing
  "Calculates the summed fitness for each test case, and then uses it to
   assign an implicit fitness sharing error to each individual. Assumes errors
   are in range [0,1] with 0 being a solution."
  [pop-agents {:keys [use-single-thread]}]
  (println "\nCalculating implicit fitness sharing errors...")
  (let [pop (map deref pop-agents)
        summed-reward-on-test-cases (map (fn [list-of-errors]
                                           (reduce +' (map #(- 1.0 %) list-of-errors)))
                                         (apply map list (map :errors pop)))]
    (println "Implicit fitness sharing reward per test case (lower means population performs worse):")
    (println summed-reward-on-test-cases)
    (assert (every? (fn [error] (< -0.0000001 error 1.0000001))
                    (flatten (map :errors pop)))
            (str "All errors must be in range [0,1]. Please normalize them. Here are the first 20 offending errors:\n"
                 (not-lazy (take 20 (filter (fn [error] (not (< 0.0 error 1.0)))
                                            (flatten (map :errors pop)))))))
    (dorun (map #((if use-single-thread swap! send)
                   %
                   assign-ifs-error-to-individual
                   summed-reward-on-test-cases)
                pop-agents))
    (when-not use-single-thread (apply await pop-agents)))) ;; SYNCHRONIZE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform selection (i.e. no selection, for use as a baseline)

(defn uniform-selection
  "Returns an individual uniformly at random."
  [pop argmap]
  (lrand-nth (age-mediate pop argmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection

(defn select
  "Returns a selected parent."
  [pop {:keys [parent-selection print-selection-counts]
        :as argmap}]
  (let [pop-with-meta-errors (map (fn [ind] (update-in ind [:errors] concat (:meta-errors ind)))
                                  pop)
        selected (case parent-selection
                   :tournament (tournament-selection pop-with-meta-errors argmap)
                   :lexicase (lexicase-selection pop-with-meta-errors argmap)
                   :epsilon-lexicase (epsilon-lexicase-selection pop-with-meta-errors argmap)
                   :elitegroup-lexicase (elitegroup-lexicase-selection pop-with-meta-errors argmap)
                   :leaky-lexicase (if (< (lrand) (:lexicase-leakage argmap))
                                     (uniform-selection pop-with-meta-errors argmap)
                                     (lexicase-selection pop-with-meta-errors argmap))
                   :uniform (uniform-selection pop-with-meta-errors)
                   (throw (Exception. (str "Unrecognized argument for parent-selection: "
                                           parent-selection))))]
    (when print-selection-counts
      (swap! selection-counts update-in [(:uuid selected)] (fn [sel-count]
                                                             (if (nil? sel-count)
                                                               1
                                                               (inc sel-count)))))
    selected))

