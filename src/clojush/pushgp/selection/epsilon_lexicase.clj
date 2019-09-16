(ns clojush.pushgp.selection.epsilon-lexicase
  (:use [clojush random globals util]))

(defn mad
  "Returns median absolute deviation (MAD)"
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
  considered one at a time in random order."
  [pop {:keys [epsilon-lexicase-epsilon epsilon-lexicase-probability] :as argmap}]
  (let [epsilons (if (empty? @epsilons-for-epsilon-lexicase) ; Normally, epsilons will be calculated
                   ; once per genertion. But, with batch-lexicase with epsilon, each selection will
                   ; have different batches, and therefore different epsilons to be calculated
                   ; once per selection.
                   (calculate-epsilons-for-epsilon-lexicase pop argmap)
                   @epsilons-for-epsilon-lexicase)]
    (loop [survivors pop
           cases (lshuffle (range (count (:errors (first pop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors))
              (< (lrand) (:lexicase-slippage argmap)))
        (lrand-nth survivors)
        (let [; If epsilon-lexicase-epsilon is set in the argmap, use it for epsilon.
              ; Otherwise, use automatic epsilon selections, which are calculated once per generation.
              epsilon (if (<= (lrand) epsilon-lexicase-probability)
                        (if epsilon-lexicase-epsilon
                          epsilon-lexicase-epsilon
                          (nth epsilons (first cases)))
                        0)
              min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))]
          (recur (filter #(<= (nth (:errors %)
                                   (first cases))
                              (+ min-err-for-case
                                 epsilon))
                         survivors)
                 (rest cases)))))))

