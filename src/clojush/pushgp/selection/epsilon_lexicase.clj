(ns clojush.pushgp.selection.epsilon-lexicase
  (:use [clojush random globals util]
        clojush.pushgp.selection.lexicase))

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
  [pop {:keys [epsilon-lexicase-epsilon]}]
  (when (not epsilon-lexicase-epsilon)
    (let [test-case-errors (apply map list (map :errors pop))
          meta-case-errors (apply map list (map :meta-errors pop))
          all-errors (vec (concat test-case-errors meta-case-errors))
          epsilons (map mad all-errors)]
      epsilons)))

(defn epsilon-lexicase-selection
  "Returns an individual that does within epsilon of the best on the fitness cases when 
  considered one at a time in random order.
  epsilon-lexicase-version can be:
   - :semi-dynamic: The standard version of epsilon-lexicase, also referred to as semi-dynamic epsilon
       lexicase. Calculates the elite error given the candidates remaining at each step
       of lexicase, but calculates epsilons only once per generation.
   - :dynamic: This version not only dynamically determines the elite error on each test case,
       but also dynamically calculates the epsilons, instead of only calculating them once
       per generation.
   - :static: This version is handled separately below.
  "
  [pop {:keys [epsilon-lexicase-epsilon epsilon-lexicase-probability epsilon-lexicase-version] :as argmap}]
  (let [epsilons (cond
                   ; Dynamic epsilon lexicase doesn't pre-calculate epsilons
                   (= epsilon-lexicase-version :dynamic) nil
                   ; For semi-dynamic, epsilons will normally be calculated
                   ; once per genertion. 
                   ; But, with batch-lexicase with epsilon, each selection will
                   ; have different batches, and therefore different epsilons to be calculated
                   ; once per selection. This is the case if @epsilons-for-epsilon-lexicase is empty.
                   (empty? @epsilons-for-epsilon-lexicase)
                   (calculate-epsilons-for-epsilon-lexicase pop argmap)
                   ; If here, using semi-dynamic and epsilons have already been calculated 
                   :else @epsilons-for-epsilon-lexicase)]
    (loop [survivors pop
           cases (shuffle-cases pop argmap)]
      (if (or (empty? cases)
              (empty? (rest survivors))
              (< (lrand) (:lexicase-slippage argmap)))
        (lrand-nth survivors)
        (let [; If epsilon-lexicase-epsilon is set in the argmap, use it for epsilon.
              ; Otherwise, use automatic epsilon selections, which are calculated once per generation.
              epsilon (cond
                        ;; Allows for only sometimes using epsilon lexicase, and
                        ;; otherwise defaulting to epsilon of 0, which is vanilla lexicase.
                        (<= (lrand) epsilon-lexicase-probability)
                        0
                        ;; If set, uses a fixed epsilon
                        epsilon-lexicase-epsilon epsilon-lexicase-epsilon
                        ;; Semi-dynamic epsilon lexicase is the recommended default version, see above
                        (= epsilon-lexicase-version :semi-dynamic)
                        (nth epsilons (first cases))
                        ;; Dynamic means to calculate the epsilons at every step of lexicase
                        (= epsilon-lexicase-version :dynamic)
                        (mad (map #(nth (:errors %) ; this calculates epsilon
                                        (first cases))
                                  survivors))
                        )
              min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))]
          (recur (filter #(<= (nth (:errors %)
                                   (first cases))
                              (+ min-err-for-case
                                 epsilon))
                         survivors)
                 (rest cases)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; static epsilon lexicase selection
;; With static epsilon lexicase, it is determined ahead of time whether each individual is within
;; epsilon of the global elite error. This makes it possible for no individuals to be within epsilon
;; of elite, in which case that test case is skipped.

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
      (println "Epsilons for static epsilon lexicase:" epsilons)
      (calculate-fitness-from-static-epsilons pop-agents elite-errors epsilons use-single-thread))))

(defn static-epsilon-lexicase-selection
  "Returns an individual that does within epsilon of the best on the fitness cases when considered one at a
   time in random order.  If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location
   With static epsilon lexicase, it is determined ahead of time whether each individual is within
   epsilon of the global elite error. This makes it possible for no individuals to be within epsilon
   of elite, in which case that test case is skipped."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)]
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
