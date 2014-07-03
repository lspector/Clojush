(ns clojush.pushgp.breed
  (:use [clojush globals random simplification individual evaluate translate]
        [clojush.pushgp parent-selection genetic-operators])
  (:require [clj-random.core :as random]))

; A map of genetic operator keywords to maps containing the genetic operator
; functions and number of parents
(def genetic-operators
  {:reproduction {:fn reproduction :parents 1}
   :alternation {:fn alternation :parents 2}
   :uniform-mutation {:fn uniform-mutation :parents 1}
   :uniform-close-mutation {:fn uniform-close-mutation :parents 1}
   :uniform-silence-mutation {:fn uniform-silence-mutation :parents 1}
   :make-next-operator-revertable {:fn nil :parents 0}
   })

(defn revert-too-big-child
  "Determines what individual should replace a child program that exceeds the
   size limit. Options are:
     :parent -- one of the parents (default)
     :empty  -- an empty program
     :random -- a random program
   In future, may implement :delete, which deletes some of the instructions
   in a parent."
  [parent {:keys [replace-child-that-exceeds-size-limit-with atom-generators
                  max-points-in-initial-program]
           :as argmap}]
  (case replace-child-that-exceeds-size-limit-with
    :parent parent
    :empty (make-individual :genome '())
    :random (make-individual :genome (random-code max-points-in-initial-program atom-generators argmap))
    ))

(defn revert-to-parent-if-worse
  "Evaluates child and parent, returning the child if it is at least as good as
   the parent on every test case."
  [child parent rand-gen {:keys [error-function parent-reversion-probability] :as argmap}]
  (let [evaluated-child (evaluate-individual (assoc child :program (translate-plush-genome-to-push-program child))
                                             error-function rand-gen argmap)
        child-errors (:errors evaluated-child)
        evaluated-parent (evaluate-individual (assoc parent :program (translate-plush-genome-to-push-program parent))
                                              error-function rand-gen argmap)
        parent-errors (:errors evaluated-parent)]
    (if (or (>= (lrand) parent-reversion-probability)
            (reduce #(and %1 %2)
                    (map <= child-errors parent-errors)))
      evaluated-child
      evaluated-parent)))

(defn perform-genetic-operator-list
  "Recursively applies the genetic operators in operator-list, using
   first-parent as the first parent for each operator call, to create a new
   child."
  [operator-list first-parent population location rand-gen argmap]
  (if (empty? operator-list)
    first-parent
    (let [revertable (= (first operator-list) :make-next-operator-revertable)
          op-list (if revertable
                    (rest operator-list)
                    operator-list)
          operator (first op-list)
          num-parents (:parents (get genetic-operators operator))
          other-parents (repeatedly (dec num-parents) #(select population location argmap))
          op-fn (:fn (get genetic-operators operator))
          child (apply op-fn (concat (vector first-parent) other-parents (vector argmap)))]
      (recur (rest op-list)
             (if revertable
               (revert-to-parent-if-worse child first-parent rand-gen argmap)
               child)
             population
             location
             rand-gen
             argmap))))

(defn perform-genetic-operator
  "Takes a single genetic operator keyword or a sequence of operator keywords,
   and performs them to create a new individual. Uses recursive helper function
   even with a single operator by putting that operator in a vector."
  [operator population location rand-gen {:keys [max-points] :as argmap}]
  (let [first-parent (select population location argmap)
        child (if (sequential? operator)
                (perform-genetic-operator-list operator first-parent population location rand-gen argmap)
                (perform-genetic-operator-list (vector operator) first-parent population location rand-gen argmap))]
    (if (> (count (:genome child)) max-points) ; Check if too big
      (revert-too-big-child first-parent argmap)
      (assoc child :parent first-parent))))

(defn breed
  "Returns an individual bred from the given population using the given parameters."
  [agt ;necessary since breed is called using swap! or send, even though not used
   location rand-gen population
   {:keys [genetic-operator-probabilities] :as argmap}]
  (random/with-rng rand-gen
    (let [prob (lrand)]
      (loop [vectored-go-probabilities (reductions #(assoc %2 1 (+ (second %1) (second %2)))
                                                   (vec genetic-operator-probabilities))]
        (if (<= prob (second (first vectored-go-probabilities)))
          (perform-genetic-operator (first (first vectored-go-probabilities)) population location rand-gen argmap)
          (recur (rest vectored-go-probabilities)))))))
