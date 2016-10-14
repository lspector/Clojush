(ns clojush.pushgp.collider
  (:use [clojush args globals individual random translate]
        [clojush.pushgp genetic-operators breed]))

(defn collider-random-individual
  "Returns a random individual using parameters in @push-argmap."
  []
  (make-individual
    :genome (random-plush-genome (:max-genome-size-in-initial-program @push-argmap)
                                 @global-atom-generators
                                 @push-argmap)))

(defn satisfies-constraints?
  [genome]
  true)

(defn lex
  "Returns a single individual from the provided population (vector of individuals) via lexicase selection."
  [popvec]
  (loop [survivors popvec
         cases (shuffle (range (count (:errors (first popvec)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn remove-one
  "Returns the provided vector v without its first instance of the provided item. Assumes that item is in v."
  [item v]
  (loop [processed []
         remaining v]
    (if (= item (first remaining))
      (vec (concat processed (rest remaining)))
      (recur (vec (conj processed (first remaining)))
             (rest remaining)))))

(defn inverse-lex
  "Returns a single individual from the provided pooulation (vector of individuals) via inverse lexicase selection."
  [popvec]
  (loop [survivors popvec
         cases (lshuffle (range (count (:errors (first popvec)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [max-err-for-case (apply max (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) max-err-for-case)
                       survivors)
               (rest cases))))))

(defn parent-selection-collision
  "Returns a parent by colliding two elements of the population and returning the one selected that lexicase
  selection selects."
  [population]
  (lex [(lrand-nth population) (lrand-nth population)]))

(defn destructive-collision
  "Returns the provided population without one individual, chosen by performing inverse lexicase selection
  on two random individuals."
  [population]
  (remove-one (inverse-lex [(rand-nth population) (rand-nth population)])
              population))

(defn collider-variation
  [parent1 parent2]
  (uniform-mutation (alternation parent1 parent2 @push-argmap) @push-argmap))

(defn constructive-collision
  "Returns the provided population with the possible addition of an individual created by running a genetic
  operator on selected parents. The new individual must bass the test of `satisfiesconstraints?` in order
  to be added. If the collision produces a solution, then instead of the augmented population, this returns
  {:solution solution-individual}."
  [population]
  (let [child (if (< (count population) (:collider-collision-threshold @push-argmap))
                (collider-random-individual)
                (let [parent1 (parent-selection-collision population)
                      parent2 (parent-selection-collision population)
                      varied (collider-variation parent1 parent2)]
                  (if (empty? (:genome varied))
                    (collider-random-individual)
                    (if (> (count (:genome varied))
                           (/ (:max-points @push-argmap)))
                      (revert-too-big-child parent1 varied @push-argmap)
                      varied))))]
    (if (satisfies-constraints? (:genome child))
      (let [program (translate-plush-genome-to-push-program child @push-argmap)
            errors ((:error-function @push-argmap) program)
            total-error (reduce + errors)
            evaluated-child (assoc child
                              :program program
                              :errors errors
                              :total-error total-error)]
        (if (<= total-error (:error-threshold @push-argmap))
          {:solution evaluated-child}
          (conj population evaluated-child)))
      population)))

(defn run-collider
  [argmap]
  (let [target-size (:collider-target-population-size @push-argmap)]
    (loop [population []
           step 0]
      (when (and (zero? (mod step 1000)) (not-empty population))
        (println "=== Report at step:" step)
        (println "Population size:" (count population))
        (println "Best total error:" (apply min (map :total-error population)))
        (println "Average total error:" (float (/ (reduce + (map :total-error population)) (count population))))
        (println "Average genome size:" (float (/ (reduce + (map count (map :genome population))) (count population)))))
      (if (:solution population)
        (println "Success at step" step ":" (:solution population))
        (if (> step 1000000)
          (println "Failure")
          (recur (if (or (empty? population)
                         (< (lrand)
                            (/ target-size (+ target-size (count population)))))
                   (constructive-collision population)
                   (destructive-collision population))
                 (inc step)))))))