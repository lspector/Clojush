
(ns clojush.pushgp.parent-selection
  (:use [clojush random globals util])
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tournament selection
(defn tournament-selection
  "Returns an individual that does the best out of a tournament."
  [pop location {:keys [tournament-size trivial-geography-radius
                        total-error-method]}]
  (let [tournament-set 
        (doall
          (for [_ (range tournament-size)]
            (nth pop
                 (if (zero? trivial-geography-radius)
                   (lrand-int (count pop))
                   (mod (+ location (- (lrand-int (+ 1 (* trivial-geography-radius 2))) trivial-geography-radius))
                        (count pop))))))
        err-fn (case total-error-method
                 :sum :total-error
                 (:hah :rmse :ifs) :weighted-error
                 (throw (Exception. (str "Unrecognized argument for total-error-method: "
                                         total-error-method))))]
    (reduce (fn [i1 i2] (if (< (err-fn i1) (err-fn i2)) i1 i2))
            tournament-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexicase selection

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
  
(defn lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
   time in random order.  If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location"
  [pop location {:keys [trivial-geography-radius] :as argmap}]
  (let [lower (mod (- location trivial-geography-radius) (count pop))
        upper (mod (+ location trivial-geography-radius) (count pop))
        popvec (vec pop)
        subpop (if (zero? trivial-geography-radius) 
                 pop
                 (if (< lower upper)
                   (subvec popvec lower (inc upper))
                   (into (subvec popvec lower (count pop)) 
                         (subvec popvec 0 (inc upper)))))]
    (loop [survivors (retain-one-individual-per-error-vector 
                       (possibly-remove-individuals-with-empty-genomes
                         subpop argmap))
           cases (lshuffle (range (count (:errors (first subpop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (lrand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map #(:errors %) survivors)))]
          (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                         survivors)
                 (rest cases)))))))

(defn mad
  "returns median absolute deviation (MAD)"
  [x]
  (let [; Get median of x
        x-median (median x)
        ; calculate absolute deviation from median
        dev (map #(Math/abs (- % x-median))
                 x)]
    (median dev)))

(defn epsilon-lexicase-selection
    "Returns an individual that does within epsilon of the best on the fitness cases when considered one at a
   time in random order.  If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location"
  [pop location {:keys [trivial-geography-radius epsilon-lexicase-epsilon]}]
  (let [lower (mod (- location trivial-geography-radius) (count pop))
        upper (mod (+ location trivial-geography-radius) (count pop))
        popvec (vec pop)
        subpop (if (zero? trivial-geography-radius)
                 pop
                 (if (< lower upper)
                   (subvec popvec lower (inc upper))
                   (into (subvec popvec lower (count pop))
                         (subvec popvec 0 (inc upper)))))]
    (loop [survivors (retain-one-individual-per-error-vector subpop)
           cases (lshuffle (range (count (:errors (first subpop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (lrand-nth survivors)
        (let [; If epsilon-lexicase-epsilon is set in the argmap, use it for epsilon.
              ; Otherwise, use automatic epsilon selections. aka use MAD for epsilon.
              epsilon (if epsilon-lexicase-epsilon
                        epsilon-lexicase-epsilon
                        (mad (map #(nth (:errors %)
                                        (first cases))
                                  survivors)))
              min-err-for-case (apply min (map #(nth % (first cases))
                                               (map #(:errors %) survivors)))]
        (recur (filter #(<= (nth (:errors %)
                                 (first cases))
                            (+ min-err-for-case
                               epsilon))
                       survivors)
               (rest cases)))))))

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
  [pop]
  (loop [survivors (retain-one-individual-per-error-vector pop)
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
  [pop]
  (lrand-nth pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection

(defn select
  "Returns a selected parent."
  [pop location {:keys [parent-selection print-selection-counts]
                 :as argmap}]
  (let [pop-with-meta-errors (map (fn [ind] (update-in ind [:errors] concat (:meta-errors ind)))
                                  pop)
        selected (case parent-selection
                   :tournament (tournament-selection pop-with-meta-errors location argmap)
                   :lexicase (lexicase-selection pop-with-meta-errors location argmap)
                   :epsilon-lexicase (epsilon-lexicase-selection pop-with-meta-errors location argmap)
                   :elitegroup-lexicase (elitegroup-lexicase-selection pop-with-meta-errors)
                   :leaky-lexicase (if (< (lrand) (:lexicase-leakage argmap))
                                     (uniform-selection pop-with-meta-errors)
                                     (lexicase-selection pop-with-meta-errors location argmap))
                   :uniform (uniform-selection pop-with-meta-errors)
                   (throw (Exception. (str "Unrecognized argument for parent-selection: "
                                           parent-selection))))]
    (when print-selection-counts
      (swap! selection-counts update-in [(:uuid selected)] (fn [sel-count]
                                                             (if (nil? sel-count)
                                                               1
                                                               (inc sel-count)))))
    selected))

