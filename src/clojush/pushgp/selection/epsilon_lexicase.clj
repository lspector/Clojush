(ns clojush.pushgp.selection.epsilon-lexicase
  (:use [clojush random globals util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; epsilon lexicase selection
;; This is the standard version of epsilon lexicase selection, sometimes called
;; "dynamic epsilon lexicase selection"

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
               (rest cases)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; static epsilon lexicase selection

(defn calculate-static-epsilon-fitness-of-individual
  "Finds the static epsilon fitness of an individual"
  [ind elite-errors epsilons]
  (let [errors (concat (:errors ind)
                       (:meta-errors ind))
        epsilon-errors (map (fn [ind-error elite-error epsilon]
                              (if (<= ind-error (+ elite-error epsilon))
                                0
                                1))
                            errors
                            elite-errors
                            epsilons)]
    (assoc ind :epsilon-errors epsilon-errors)))

(defn calculate-fitness-from-static-epsilons
  "Calculates fitness for each individual as 0 if individual is within epsilon of elite, and 1 otherwise."
  [pop-agents elite-errors epsilons use-single-thread]
  (dorun (map #((if use-single-thread swap! send)
                 %
                 calculate-static-epsilon-fitness-of-individual
                 elite-errors
                 epsilons)
              pop-agents))
  (when-not use-single-thread (apply await pop-agents)))

(defn calculate-fitness-for-static-epsilon-lexicase
  "Calculates the epsilon values for epsilon lexicase selection. Only runs once
   per generation."
  [pop-agents {:keys [epsilon-lexicase-epsilon use-single-thread]}]
  (when (not epsilon-lexicase-epsilon)
    (let [pop (map deref pop-agents)
          test-case-errors (apply map list (map :errors pop))
          meta-case-errors (apply map list (map :meta-errors pop))
          all-errors (concat test-case-errors meta-case-errors)
          elite-errors (map #(apply min %) all-errors)
          epsilons (map mad all-errors)]
      (println "Epsilons for epsilon lexicase:" epsilons)
      (calculate-fitness-from-static-epsilons pop-agents elite-errors epsilons use-single-thread))))

(defn static-epsilon-lexicase-selection
  "Returns an individual that does within epsilon of the best on the fitness cases when considered one at a
   time in random order.  If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location
   With static epsilon lexicase, it is determined ahead of time whether each individual is within
   epsilon of the global elite error. This makes it possible for no individuals to be within epsilon
   of elite, in which case that test case is skipped."
  [pop]
  (loop [survivors (retain-one-individual-per-error-vector pop)
         cases (lshuffle (range (count (:epsilon-errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:epsilon-errors %) survivors)))]
        (if (not (zero? min-err-for-case))
          (recur survivors (rest cases))
          (recur (filter #(zero? (nth (:epsilon-errors %)
                                      (first cases)))
                         survivors)
                 (rest cases)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; super-dynamic epsilon lexicase selection
;; This version not only dynamically determines the elite error on each test case,
;; but also dynamically calculates the epsilons, instead of only calculating them once
;; per generation

(defn super-dynamic-epsilon-lexicase-selection
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
