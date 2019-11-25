(ns clojush.pushgp.selection.fitness-proportionate
  (:use [clojush util random]))

(defn assign-selection-probability
  "Takes an individual and assigns its selection probability."
  [ind sum-fitness+]
  (let [fitness+ (/ 1 (inc (:total-error ind)))]
    (assoc ind :fitness-proportionate-selection-probability (float (/ fitness+ sum-fitness+)))))

(defn calculate-fitness-proportionate-probabilities
  "Calculates the probabilities used for fitness-proportionate selection."
  [pop-agents {:keys [use-single-thread]}]
  (println "\nCalculating fitness-proportionate probabilities.")
  (let [pop (map deref pop-agents)
        sum-fitness+ (apply +'
                            (map #(/ 1 (inc (:total-error %)))
                                 pop))]
    (dorun (map #((if use-single-thread swap! send)
                  %
                  assign-selection-probability
                  sum-fitness+)
                pop-agents))
    (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
    (println "\nFitness-proportionate selection probabilities:" (map :fitness-proportionate-selection-probability (map deref pop-agents)))))

(defn fitness-proportionate-selection
  "Selection with probability of selection explicitly defined. Probability is
   calculated as follows:
    fitness_plus[i] = 1 / (1 + total_error[i])
    probability_of_selection[i] = fitness_plus[i] / sum_j(fitness_plus[j])"
  [pop argmap]
  (let [max-prob (apply +' (map :fitness-proportionate-selection-probability pop))]
    (loop [pop pop
           random-prob (* max-prob (lrand))]
      (let [current-prob (:fitness-proportionate-selection-probability (first pop))]
        (if (or (< random-prob current-prob)
                (empty? (rest pop)))
          (first pop)
          (recur (rest pop)
                 (- random-prob current-prob)))))))
