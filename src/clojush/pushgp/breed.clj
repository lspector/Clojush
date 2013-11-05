(ns clojush.pushgp.breed
  (:use [clojush.globals]
        [clojush.random]
        [clojush.pushgp.parent-selection]
        [clojush.pushgp.genetic-operators]
        [clojush.simplification])
  (:require [clj-random.core :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genetic operators

(defn breed
  "Replaces the state of the given agent with an individual bred from the given population, 
   using the given parameters."
  [agt location rand-gen population
   {:keys [;; genetic operator probabilities
           mutation-probability crossover-probability simplification-probability
           gaussian-mutation-probability boolean-gsxover-probability
           deletion-mutation-probability parentheses-addition-mutation-probability
           tagging-mutation-probability tag-branch-mutation-probability
           ultra-probability
           ;; Used by select
           tournament-size trivial-geography-radius
           ;; Used by simplification
           error-function reproduction-simplifications maintain-ancestors
           ]
    :as argmap}]
  (random/with-rng rand-gen
    (let [n (lrand)
          parent (select population location argmap)]
      (assoc
        (cond
          ;; mutation
          (< n mutation-probability)
          (mutate parent argmap)
          ;; crossover
          (< n (+ mutation-probability crossover-probability))
          (let [second-parent (select population location argmap)]
            (crossover parent second-parent argmap))
          ;; simplification
          (< n (+ mutation-probability crossover-probability simplification-probability))
          (auto-simplify parent error-function reproduction-simplifications false 1000 maintain-ancestors)
          ;; gaussian mutation
          (< n (+ mutation-probability crossover-probability simplification-probability 
                  gaussian-mutation-probability))
          (gaussian-mutate parent argmap)
          ;; boolean gsxover
          (< n (+ mutation-probability crossover-probability simplification-probability 
                  gaussian-mutation-probability boolean-gsxover-probability))
          (let [second-parent (select population location argmap)]
            (boolean-gsxover parent second-parent argmap))
          ;; deletion mutation
          (< n (+ mutation-probability crossover-probability simplification-probability 
                  gaussian-mutation-probability boolean-gsxover-probability deletion-mutation-probability))
          (delete-mutate parent argmap)
          ;; parentheses addition mutation
          (< n (+ mutation-probability crossover-probability simplification-probability 
                  gaussian-mutation-probability boolean-gsxover-probability
                  deletion-mutation-probability parentheses-addition-mutation-probability))
          (add-parentheses-mutate parent argmap)
          ;; tagging mutation
          (< n (+ mutation-probability crossover-probability simplification-probability 
                  gaussian-mutation-probability boolean-gsxover-probability
                  deletion-mutation-probability parentheses-addition-mutation-probability
                  tagging-mutation-probability))
          (tagging-mutate parent @global-tag-limit argmap)
          ;; tag branch mutation
          (< n (+ mutation-probability crossover-probability simplification-probability 
                  gaussian-mutation-probability boolean-gsxover-probability
                  deletion-mutation-probability parentheses-addition-mutation-probability
                  tagging-mutation-probability tag-branch-mutation-probability))        
          (tag-branch-insertion-mutate parent @global-tag-limit argmap)
          ;; ultra
          (< n (+ mutation-probability crossover-probability simplification-probability 
                  gaussian-mutation-probability boolean-gsxover-probability
                  deletion-mutation-probability parentheses-addition-mutation-probability
                  tagging-mutation-probability tag-branch-mutation-probability
                  ultra-probability))
          (let [second-parent (select population location argmap)]
            (ultra parent second-parent argmap))
          ;; replication
          true
          parent)
        :parent parent))))
