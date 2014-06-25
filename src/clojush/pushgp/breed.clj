(ns clojush.pushgp.breed
  (:use [clojush.globals]
        [clojush.random]
        [clojush.pushgp.parent-selection]
        [clojush.pushgp.genetic-operators]
        [clojush.simplification])
  (:require [clj-random.core :as random]))

(defn breed
  "Replaces the state of the given agent with an individual bred from the given population, 
   using the given parameters."
  [agt location rand-gen population
   {:keys [;; genetic operator probabilities
           uniform-mutation-probability uniform-close-mutation-probability
           ]
    :as argmap}]
  (random/with-rng rand-gen
    (let [n (lrand)
          parent (select population location argmap)]
      (assoc
        (cond
          ;; uniform mutation
          (< n uniform-mutation-probability)
          (uniform-mutation parent argmap)
          ;; uniform close mutation
          (< n (+ uniform-mutation-probability uniform-close-mutation-probability))
          (uniform-close-mutation parent argmap)
          ;; replication
          true
          parent)
        :parent parent))))
