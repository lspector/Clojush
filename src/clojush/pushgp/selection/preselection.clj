(ns clojush.pushgp.selection.preselection
  (:use [clojush random globals]))

(defn one-individual-per-error-vector-for-lexicase
  "When :parent-selection is a lexicase method, returns only one random individual 
  to represent each error vector."
  [pop {:keys [parent-selection]}]
  (if (some #{parent-selection}
            #{:lexicase :leaky-lexicase :elitegroup-lexicase})
    (map lrand-nth (vals (group-by #(:errors %) pop)))
    pop))

(defn nonempties-for-autoconstruction
  "When :autoconstuctive is truthy, and at least one individual in pop has a non-empty
  genome, returns only those individuals with non-empty genomes."
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
  returns individuals in pop with the minimum age; with probability pmax, returns all of pop;
  with probability (- 1.0 pmin pmax), selects an age cutoff uniformly from those present
  in the population and returns individuals with the cutoff age or lower. If a third
  element of :invert is included in age-mediated-parent-selection then with probability
  pmin, returns individuals in pop with the maximum age; with probability pmax, returns 
  all of pop; with probability (- 1.0 pmin pmax), selects an age cutoff uniformly from
  those present in the population and returns individuals with the cutoff age or higher."
  [pop {:keys [age-mediated-parent-selection]}]
  (if (not age-mediated-parent-selection)
    pop
    (let [rand-val (lrand)
          amps age-mediated-parent-selection ;; just abbreviate
          invert (> (count amps) 2)] ;; assume any more args are just :invert
      (if (<= rand-val (first amps))
        (let [extreme-age (reduce (if invert max min) (map :age pop))]
          (filter #(= (:age %) extreme-age) pop))
        (if (<= rand-val (+ (first amps) (second amps)))
          pop
          (let [age-limit (lrand-nth (distinct (map :age pop)))]
            (filter (fn [ind] ((if invert >= <=) (:age ind) age-limit))
                    pop)))))))

(defn screen
  "If random-screen is falsy, returns pop. Otherwise, random-screen should be a map with
  values for :criterion and :probability. Then, with probability (- 1 :probability), again
  returns pop. Otherwise, a value is chosen randomly from the :grain-size values of
  the individuals in pop, and returns the individuals with that :grain-size or smaller."
  [pop {:keys [random-screen]}]
  (if (not random-screen)
    pop
    (if (> (lrand) (:probability random-screen))
      pop
      (let [grain-size-limit (lrand-nth (distinct (map :grain-size pop)))]
        (filter (fn [ind] ((if (:reversible random-screen)
                             (lrand-nth [<= >=])
                             <=)
                           (:grain-size ind) 
                           grain-size-limit))
                pop)))))

(defn knock-off-chip-off-the-old-block
  "If (:knock-off-chip-off-the-old-block argmap) is true, then if any individual in
  pop has an error vector that is different from its mother's, then return pop without
  any individuals with error vectors identical to their mother's. Otherwise return pop 
  unchanged. If the value is a vector of the form [diffs outof] then instead of the
  requirement being that the error vector must be diffrent from its mother's, it is
  that there must be at least diffs many different error vectors in the most recent
  outof many. If diffs is :random, then it is chosen randomly from the range from 1
  to outof. If outof is also :random, then the value should actually be a vector
  of the form [:random :random limit], and outof will be chosen from the range from
  1 to limit. If the value is of the form [:random :random limit minfrac] then diffs
  will be chosen randomly from the range of (int (* minfrac outof)) to outof.
  If the value is of the form [:random :random limit minfrac maxfrac] then diffs
  will be chosen randomly from the range of (int (* minfrac outof)) to
  (int (* maxfrac outof)). If the first item in (:knock-off-chip-off-the-old-block argmap)
  is :subset, then the remainder are interpreted as above, but only a random subset
  of errors is considered in the filtering. If it is :single, then only
  a random single error. If the first item in is :min2, then the remainder are 
  interpreted as above, but the minimum for random values is 2 rather than 1."
  [pop argmap]
  (let [knock-spec (:knock-off-chip-off-the-old-block argmap)]
    (if (not knock-spec)
      pop
      (if (not (:print-history argmap))
        (throw
         (Exception.
          ":print-history must be true for :knock-off-chip-off-the-old-block"))
        (let [knock-spec (if (= true knock-spec) [2 2] knock-spec)
              min2? (= :min2 (first knock-spec))
              knock-spec (if min2? (rest knock-spec) knock-spec)
              subset? (= :subset (first knock-spec))
              knock-spec (if subset? (rest knock-spec) knock-spec)
              single? (= :single (first knock-spec))
              knock-spec (if single? (rest knock-spec) knock-spec)
              filtered-history
              (if (or subset? single?)
                (let [all (shuffle (range (count (first (:history (first pop))))))
                      keepers (cons (first all)
                                    (if single?
                                      []
                                      (vec (take (rand (count all))
                                                 (rest all)))))]
                  (fn [ind]
                    (if (empty? (:history ind))
                      []
                      (mapv (fn [errs]
                              (vec (for [i keepers] (nth errs i))))
                            (:history ind)))))
                :history)
              diffs (first knock-spec)
              outof (second knock-spec)
              limit (if (= outof :random) (nth knock-spec 2) nil)
              outof (if (= outof :random)
                      (if min2?
                        (inc (inc (lrand-int (dec limit))))
                        (inc (lrand-int limit)))
                      outof)
              diffs (if (= diffs :random)
                      (inc (if (> (count knock-spec) 3)
                             (let [mindiff (int (* (nth knock-spec 3) outof))
                                   maxdiff (if (> (count knock-spec) 4)
                                             (int (* (nth knock-spec 4) outof))
                                             outof)]
                               (+ mindiff (lrand-int (max 1 (- maxdiff mindiff)))))
                             (if min2?
                               (inc (lrand-int (dec outof)))
                               (lrand-int outof))))
                      diffs)
              changed (vec (filter (fn [ind]
                                     (or (< (count (:history ind)) diffs)
                                         (let [hist (filtered-history ind)
                                               case-hists (apply mapv list hist)]
                                           (some (fn [h]
                                                   (>= (count (distinct (take outof h)))
                                                       diffs))
                                                 case-hists))))
                                   pop))]
          (if (empty? changed)
            (do (println "Universal violation of knock-off-chip-off-the-old-block constraint.")
                pop)
            changed))))))

(defn preselect
  "Returns the population pop reduced as appropriate considering the settings for
  age-mediation, screening, selection method, and autoconstruction."
  [pop argmap]
  (-> pop
      (nonempties-for-autoconstruction argmap)
      (age-mediate argmap)
      (screen argmap)
      (knock-off-chip-off-the-old-block argmap)
      ((fn [subpop argmap]
         (when (:print-preselection-fraction argmap)
           (swap! preselection-counts #(conj % (count subpop))))
         subpop)
       argmap)
      (one-individual-per-error-vector-for-lexicase argmap)))

;;;;;;;
;; Batching for batch-lexicase.
;; Not really pre-selection, but seemed like best place for now.

(defn batch-errors-of-individual
  "Takes an individual and batches its errors in the right batches, sums them, and assocs
  to the :errors key."
  [error-indices ind {:keys [case-batch-size total-error-method] :as argmap}]
  (assoc ind :errors
         (let [ordered-errors (map #(nth (get ind
                                              (case total-error-method
                                                :sum :errors
                                                :eliteness :eliteness
                                                nil))
                                         %)
                                   error-indices)
               batched-errors (partition case-batch-size ordered-errors)
               aggregated-batches (map (partial apply +') batched-errors)]
           aggregated-batches)))

(defn batch-errors
  "Used for batch lexicase (and any other batch-based selection).
  Takes errors and places them into random batches of size case-batch-size, and then
  sums each batch. This replaces the :errors in each individual with a new error vector composed
  of the batch sums."
  [pop argmap]
  (let [shuffled-error-indices (lshuffle (range (count (:errors (first pop)))))]
    (map #(batch-errors-of-individual shuffled-error-indices % argmap)
         pop)))
