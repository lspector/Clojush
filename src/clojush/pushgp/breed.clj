(ns clojush.pushgp.breed
  (:use [clojush.random]
        [clojush.pushgp.parent-selection]
        [clojush.pushgp.genetic-operators]
        [clojush.simplification]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genetic operators

(defn breed
  "Replaces the state of the given agent with an individual bred from the given population (pop), 
   using the given parameters."
  [agt location rand-gen pop error-function population-size max-points atom-generators 
   mutation-probability  mutation-max-points crossover-probability simplification-probability 
   tournament-size reproduction-simplifications trivial-geography-radius
   gaussian-mutation-probability gaussian-mutation-per-number-mutation-probability 
   gaussian-mutation-standard-deviation boolean-gsxover-probability
   boolean-gsxover-new-code-max-points]
  (binding [*thread-local-random-generator* rand-gen]
    (let [n (lrand)]
      (cond 
        ;; mutation
        (< n mutation-probability)
        (mutate (select pop tournament-size trivial-geography-radius location) 
                mutation-max-points max-points atom-generators)
        ;; crossover
        (< n (+ mutation-probability crossover-probability))
        (let [first-parent (select pop tournament-size trivial-geography-radius location)
              second-parent (select pop tournament-size trivial-geography-radius location)]
          (crossover first-parent second-parent max-points))
        ;; simplification
        (< n (+ mutation-probability crossover-probability simplification-probability))
        (auto-simplify (select pop tournament-size trivial-geography-radius location)
                       error-function reproduction-simplifications false 1000)
        ;; gaussian mutation
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability))
        (gaussian-mutate (select pop tournament-size trivial-geography-radius location) 
                         gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation)
        ;; boolean gsxover
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability boolean-gsxover-probability))
        (let [first-parent (select pop tournament-size trivial-geography-radius location)
              second-parent (select pop tournament-size trivial-geography-radius location)]
          (boolean-gsxover first-parent second-parent boolean-gsxover-new-code-max-points max-points atom-generators))
        ;; replication
        true 
        (select pop tournament-size trivial-geography-radius location)))))
