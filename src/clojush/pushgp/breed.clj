(ns clojush.pushgp.breed
  (:use [clojush.globals]
        [clojush.random]
        [clojush.pushgp.parent-selection]
        [clojush.pushgp.genetic-operators]
        [clojush.simplification]))

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
  (binding [*thread-local-random-generator* rand-gen]
    (let [n (lrand)]
      (cond 
        ;; mutation
        (< n mutation-probability)
        (let [parent (select population location argmap)]
          (assoc (mutate parent argmap) :parent parent))
        ;; crossover
        (< n (+ mutation-probability crossover-probability))
        (let [first-parent (select population location argmap)
              second-parent (select population location argmap)]
          (assoc (crossover first-parent second-parent argmap) :parent first-parent))
        ;; simplification
        (< n (+ mutation-probability crossover-probability simplification-probability))
        (let [parent (select population location argmap)]
          (assoc (auto-simplify parent error-function reproduction-simplifications false 1000 maintain-ancestors)
                 :parent parent))
        ;; gaussian mutation
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability))
        (let [parent (select population location argmap)]
          (assoc (gaussian-mutate parent argmap) :parent parent))
        ;; boolean gsxover
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability boolean-gsxover-probability))
        (let [first-parent (select population location argmap)
              second-parent (select population location argmap)]
          (assoc (boolean-gsxover first-parent second-parent argmap) :parent first-parent))
        ;; deletion mutation
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability boolean-gsxover-probability deletion-mutation-probability))
        (let [parent (select population location argmap)]
          (assoc (delete-mutate parent argmap) :parent parent))
        ;; parentheses addition mutation
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability boolean-gsxover-probability 
                deletion-mutation-probability parentheses-addition-mutation-probability))
        (let [parent (select population location argmap)]
          (assoc (add-parentheses-mutate parent argmap) :parent parent))
        ;; tagging mutation
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability boolean-gsxover-probability 
                deletion-mutation-probability parentheses-addition-mutation-probability
                tagging-mutation-probability))
        (let [parent (select population location argmap)]
          (assoc (tagging-mutate parent @global-tag-limit argmap) :parent parent))
        ;; tag branch mutation
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability boolean-gsxover-probability 
                deletion-mutation-probability parentheses-addition-mutation-probability
                tagging-mutation-probability tag-branch-mutation-probability))        
        (let [parent (select population location argmap)]
          (assoc (tag-branch-insertion-mutate parent @global-tag-limit argmap) 
                 :parent parent))
        ;; ultra
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability boolean-gsxover-probability 
                deletion-mutation-probability parentheses-addition-mutation-probability
                tagging-mutation-probability tag-branch-mutation-probability
                ultra-probability))   
        (let [first-parent (select population location argmap)
              second-parent (select population location argmap)]
          (assoc (ultra first-parent second-parent argmap) :parent first-parent))
        ;; replication
        true 
        (let [parent (select population location argmap)]
          (assoc parent :parent parent))))))
