(ns clojush.pushgp.collider
  (:use [clojush args globals individual random translate]
        [clojush.pushgp genetic-operators breed])
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
         cases (shuffle (range (count (:errors (first popvec)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [max-err-for-case (apply max (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) max-err-for-case)
                       survivors)
               (rest cases))))))

;(defn parent-selection-collision
;  "Returns a parent by colliding two elements of the population and returning the one selected that lexicase
;  selection selects."
;  [population]
;  (lex [(rand-nth population) (rand-nth population)]))

(defn destructive-collision
  "Returns a collection containing only one of the two given individuals, the one that wins a lexicase selection
  tournament."
  [[i1 i2]]
  [(lex [i1 i2])])

(defn collider-variation
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
  [args] ;; args should be :from-scratch or a collection of 4 individuals
  (let [child (if (= args :from-scratch)
                (collider-random-individual)
                (let [parent1 (lex [(nth args 0) (nth args 1)])
                      parent2 (lex [(nth args 2) (nth args 3)])
                      varied (collider-variation parent1 parent2)]
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
        (if (= args :from-scratch)
          [evaluated-child]
          (conj args evaluated-child)))
      (if (= args :from-scratch)
        []
        args))))

(def point-evaluations-at-last-collider-report (atom 0))

(def collider-pool (atom nil))

(defn mapper
  []
  (if (:use-single-thread @push-argmap)
    map
    (partial cp/pmap @collider-pool)))

(defn run-collider
  [argmap]
  (let [multithread (not (:use-single-thread @push-argmap))]
    (reset! solution nil)
    (reset! point-evaluations-at-last-collider-report 0)
    (when multithread
      (reset! collider-pool (cp/threadpool (:collider-simultaneous-collisions @push-argmap))))
    (let [target-size (:collider-target-population-size @push-argmap)
          simultaneous-collisions (if multithread (:collider-simultaneous-collisions @push-argmap) 1)]
      (loop [population []]
        (when (and (not-empty population)
                   (> @point-evaluations-count
                      (+ @point-evaluations-at-last-collider-report
                         (:collider-point-evaluations-per-report @push-argmap))))
          (println "=== Report at point evaluations:" @point-evaluations-count)
          (println "Population size:" (count population))
          (println "Best total error:" (apply min (map :total-error population)))
          (println "Average total error:" (float (/ (reduce + (map :total-error population)) (count population))))
          (println "Average genome size:" (float (/ (reduce + (map count (map :genome population))) (count population))))
          (reset! point-evaluations-at-last-collider-report @point-evaluations-count))
        (if @solution
          (do (println "Success at point evaluations:" @point-evaluations-count @solution)
              (when multithread (cp/shutdown @collider-pool)))
          (if (>= @point-evaluations-count (:max-point-evaluations @push-argmap))
            (do (println "Failure")
                (when multithread (cp/shutdown @collider-pool)))
            (if (< (count population) (:collider-collision-threshold @push-argmap))
              (recur (concat population (constructive-collision :from-scratch)))
              (let [shuffled-population (shuffle population)
                    construction-ratio (/ target-size (+ target-size (count population)))
                    collisions (repeatedly simultaneous-collisions
                                           #(if (< (rand) construction-ratio) :constructive :destructive))
                    constructive (count (filter #{:constructive} collisions))
                    destructive (- (count collisions) constructive)]
                (recur (concat (drop (+ (* 4 constructive) (* 2 destructive)) shuffled-population)
                               (apply concat (map destructive-collision
                                                  (partition 2 (take (* 2 destructive) shuffled-population))))
                               (apply concat ((mapper) constructive-collision
                                               (partition 4 (take (* 4 constructive)
                                                                  (drop (* 2 destructive) shuffled-population)))))))))))))))