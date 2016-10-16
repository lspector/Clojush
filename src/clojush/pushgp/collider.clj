(ns clojush.pushgp.collider
  (:use [clojush args globals individual random translate]
        [clojush.pushgp genetic-operators breed report])
  (:require [com.climate.claypoole :as cp]))

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
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn remove-one
  "Returns the provided collection coll without its first instance of the provided item. Assumes that item is in coll."
  [item coll]
  (loop [processed []
         remaining coll]
    (if (= item (first remaining))
      (concat processed (rest remaining))
      (recur (conj processed (first remaining))
             (rest remaining)))))

(defn destructive-collision
  "Returns the given individuals without the loser of an iterated lexicase selection process."
  [pool]
  (loop [remaining pool
         result []]
    (if (= (count remaining) 1)
      result
      (let [winner (lex remaining)]
        (recur (remove-one winner remaining)
               (conj result winner))))))

(defn collider-variation
  "Returns an individual that is a recombination/mutation of the provided 2 parents."
  [parent1 parent2]
  (-> (alternation parent1 parent2 @push-argmap)
      (uniform-mutation @push-argmap)
      (uniform-close-mutation @push-argmap)
      (uniform-silence-mutation @push-argmap)
      (uniform-deletion @push-argmap)
      (uniform-addition @push-argmap)))

(def solution (atom nil))

(defn constructive-collision
  "Returns the provided individuals with the possible addition of an individual created by varying them. The new
  individual must bass the test of `satisfiesconstraints?` in order to be returned. If the collision produces a
  solution, then it is stored in the solution atom."
  [args] ;; args should be :from-scratch or a collection of 3 individuals
  (let [from-scratch (= args :from-scratch)
        parent1 (if from-scratch nil (lex args))
        parent2 (if from-scratch nil (lex (remove-one parent1 args)))
        child (if from-scratch
                (collider-random-individual)
                (let [varied (collider-variation parent1 parent2)]
                  (if (empty? (:genome varied))
                    (collider-random-individual)
                    (if (> (count (:genome varied))
                           (:max-points @push-argmap))
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
        (when (<= total-error (:error-threshold @push-argmap))
          (reset! solution evaluated-child))
        (if from-scratch
          [evaluated-child]
          (conj args evaluated-child)))
      (if from-scratch [] args))))

(def point-evaluations-at-last-collider-report (atom 0))

(def collider-pool (atom nil))

(defn mapper
  []
  (partial cp/upmap (if (:use-single-thread @push-argmap) :serial @collider-pool)))

(defn run-collider
  [argmap]
  (reset! solution nil)
  (reset! point-evaluations-at-last-collider-report 0)
  (reset! collider-pool (cp/threadpool (:collider-threads @push-argmap)))
  (let [target-size (:collider-target-population-size @push-argmap)
        arity (:collider-arity @push-argmap)]
    (loop [population []]
      (when (and (not-empty population)
                 (> @point-evaluations-count
                    (+ @point-evaluations-at-last-collider-report
                       (:collider-point-evaluations-per-report @push-argmap))))
        (println "=== Collider report at point evaluations:" @point-evaluations-count)
        (println "Collider population size:" (count population))
        (report-and-check-for-success population @point-evaluations-count @push-argmap)
        (reset! point-evaluations-at-last-collider-report @point-evaluations-count))
      (if @solution
        (do (println "Success at point evaluations:" @point-evaluations-count @solution)
            (cp/shutdown @collider-pool))
        (if (>= @point-evaluations-count (:max-point-evaluations @push-argmap))
          (do (println "Failure")
              (cp/shutdown @collider-pool))
          (if (< (count population) (/ (:collider-target-population-size @push-argmap) 2))
            (recur (concat population
                           (apply concat
                                  (doall ((mapper) constructive-collision
                                           (repeat (:collider-threads @push-argmap) :from-scratch))))))
            (let [shuffled-population (shuffle population)
                  construction-ratio (/ target-size (+ target-size (count population)))
                  collisions (repeatedly (int (/ (count population) arity))
                                         #(if (< (rand) construction-ratio)
                                           constructive-collision
                                           destructive-collision))]
              (recur (concat (drop (* arity (count collisions)) shuffled-population)
                             (apply concat
                                    (doall ((mapper)
                                             #(%1 %2)
                                             collisions
                                             (partition arity shuffled-population)))))))))))))