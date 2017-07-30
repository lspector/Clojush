(ns clojush.graphs.config.population
  (:require [plumbing.core :refer [defnk]]
            [plumbing.graph]
            [clojure.repl :as repl]

            [clojush.individual :refer [make-individual]]
            [clojush.random :as random]))

(defn agent-error-handler
  "Given to individual agents for handling errors."
  [agnt except]
  ;(.printStackTrace except System/out)
  ;(.printStackTrace except)
  (repl/pst except 10000)
  (System/exit 0))

(defn strip-random-insertion-flags
 "The :random-insertion flag is added to all elements of the
  genome when generated. It is used to signal that an
  instruction-map was generated randomly in the run (as opposed
  to being mutated from a parent). The individuals in generation
  0 are a special case and should not have this flag present."
  [genome]
  (mapv #(dissoc % :random-insertion) genome))


(defnk pop-agents
  "Makes the population of agents containing the initial random individuals in the population."
  [argmap]
  (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println "\nGenerating initial population...") (flush)
  (let [population-agents (repeatedly (:population-size argmap)
                                      #(make-individual
                                         :genome (strip-random-insertion-flags
                                                   (random/random-plush-genome
                                                     (:max-genome-size-in-initial-program argmap)
                                                     (:atom-generators argmap)
                                                     argmap))
                                         :genetic-operators :random))]
    (mapv #(if (:use-single-thread argmap)
             (atom %)
             (agent % :error-handler agent-error-handler))
          population-agents)))

(defnk child-agents
  "Makes the population of agents containing the initial random individuals in the population."
  [pop-agents argmap]
  (vec (repeatedly (:population-size argmap)
                   #((if (:use-single-thread argmap) atom agent)
                     (make-individual)
                     :error-handler agent-error-handler))))

(def graph
  (plumbing.graph/graph
    pop-agents
    child-agents))
