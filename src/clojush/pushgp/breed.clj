(ns clojush.pushgp.breed
  (:use [clojush globals random simplification individual evaluate translate]
        [clojush.pushgp genetic-operators]
        [clojush.pushgp.selection selection])
  (:require [clj-random.core :as random]))

; A map of genetic operator keywords to maps containing the genetic operator
; functions and number of parents
(def genetic-operators
  {:reproduction {:fn reproduction :parents 1 :works-with-plushy true :works-with-plush true}
   :alternation {:fn alternation :parents 2 :works-with-plushy true :works-with-plush true}
   :two-point-crossover {:fn two-point-crossover :parents 2 :works-with-plushy true :works-with-plush true}
   :uniform-crossover {:fn uniform-crossover :parents 2 :works-with-plushy true :works-with-plush true}
   :uniform-mutation {:fn uniform-mutation :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-instruction-mutation {:fn uniform-instruction-mutation :parents 1 :works-with-plush true}
   :uniform-integer-mutation {:fn uniform-integer-mutation :parents 1 :works-with-plush true}
   :uniform-float-mutation {:fn uniform-float-mutation :parents 1 :works-with-plush true}
   :uniform-tag-mutation {:fn uniform-tag-mutation :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-string-mutation {:fn uniform-string-mutation :parents 1 :works-with-plush true}
   :uniform-boolean-mutation {:fn uniform-boolean-mutation :parents 1 :works-with-plush true}
   :uniform-close-mutation {:fn uniform-close-mutation :parents 1 :works-with-plush true}
   :uniform-silence-mutation {:fn uniform-silence-mutation :parents 1 :works-with-plush true}
   :uniform-deletion {:fn uniform-deletion :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-addition {:fn uniform-addition :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-addition-and-deletion {:fn uniform-addition-and-deletion :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-combination {:fn uniform-combination :parents 2 :works-with-plushy true :works-with-plush true}
   :uniform-combination-and-deletion {:fn uniform-combination-and-deletion :parents 2 :works-with-plushy true :works-with-plush true}
   :genesis {:fn genesis :parents 0 :works-with-plushy true :works-with-plush true} ;; the parent will be ignored
   :make-next-operator-revertable {:fn nil :parents 0 :works-with-plushy true :works-with-plush true}
   :autoconstruction {:fn autoconstruction :parents 2 :works-with-plush true}
   :gene-selection {:fn gene-selection :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-reordering {:fn uniform-reordering :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-segment-reordering {:fn uniform-segment-reordering :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-segment-transposition {:fn uniform-segment-transposition :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-segment-duplication {:fn uniform-segment-duplication :parents 1 :works-with-plushy true :works-with-plush true}
   :uniform-segment-deletion {:fn uniform-segment-deletion :parents 1 :works-with-plushy true :works-with-plush true}
   })

(defn revert-too-big-child
  "Determines what individual should replace a child program that exceeds the
   size limit. Options are:
     :parent -- one of the parents
     :empty  -- an empty program
     :truncate -- truncate child after max points
     :random -- a random program
   In future, may implement :delete, which deletes some of the instructions
   in a parent."
  [parent child {:keys [replace-child-that-exceeds-size-limit-with atom-generators
                        max-genome-size-in-initial-program max-points genome-representation]
                 :as argmap}]
  (case replace-child-that-exceeds-size-limit-with
    :parent parent
    :empty (make-individual :genome [] :genetic-operators :empty)
    :truncate (assoc child :genome (vec (take (/ max-points 4) (:genome child))))
    :random (genesis argmap)))

(defn revert-to-parent-if-worse
  "Evaluates child and parent, returning the child if it is at least as good as
   the parent on every test case."
  [child parent rand-gen {:keys [error-function parent-reversion-probability] :as argmap}]
  (let [evaluated-child (evaluate-individual 
                          (assoc child :program (translate-plush-genome-to-push-program child argmap))
                          error-function rand-gen argmap)]
    (if (>= (lrand) parent-reversion-probability)
      evaluated-child
      (let [child-errors (:errors evaluated-child)
            evaluated-parent (evaluate-individual 
                               (assoc parent :program 
                                 (translate-plush-genome-to-push-program parent argmap))
                               error-function rand-gen argmap)
            parent-errors (:errors evaluated-parent)]
        (if (reduce #(and %1 %2)
                    (map <= child-errors parent-errors))
          evaluated-child
          evaluated-parent)))))

(defn perform-genetic-operator-list
  "Recursively applies the genetic operators in operator-list, using
   first-parent as the first parent for each operator call, to create a new
   child."
  ([operator-list first-parent population location rand-gen argmap]
   (perform-genetic-operator-list operator-list first-parent population location rand-gen argmap
                                  (remove nil? (list first-parent))))
  ([operator-list first-parent population location rand-gen argmap parents]
   (if (empty? operator-list)
     (assoc first-parent ; first-parent, in this case, is the child individual
            :parent-uuids (vec (map :uuid parents))
            :age (cond
                   (empty? parents) 0
                   (= (count parents) 1) (inc (:age (first parents)))
                   (= (count parents) 2) ((age-combining-function argmap)
                                          (first parents) (second parents) (:genome first-parent))
                   :else "Don't know how to combine ages of more than 2 parents."))
     (let [revertable (= (first operator-list) :make-next-operator-revertable)
           op-list (if revertable
                     (rest operator-list)
                     operator-list)
           operator (first op-list)
           num-parents (:parents (get genetic-operators operator))
           other-parents (vec (repeatedly 
                               (dec num-parents) 
                               (fn []
                                 (loop [re-selections 0
                                        other (select population argmap)]
                                   (if (and (= other first-parent)
                                            (< re-selections 
                                               (:self-mate-avoidance-limit argmap)))
                                     (recur (inc re-selections)
                                            (select population argmap))
                                     other)))))
           op-fn (:fn (get genetic-operators operator))
           child (apply op-fn (concat (if (nil? first-parent)
                                        nil
                                        (vector first-parent))
                                      other-parents
                                      (vector (assoc argmap
                                                     :population population))))]
       (recur (rest op-list)
              (if revertable
                (revert-to-parent-if-worse child first-parent rand-gen argmap)
                child)
              population
              location
              rand-gen
              argmap
              (concat parents other-parents))))))

(defn update-instruction-map-uuids
  "Takes an individual and updates the UUIDs on every instruction-map in its
   :genome, except for the ones which are a random insertion."
  [individual]
  (update individual :genome
          (fn [genome]
            (mapv (fn [instruction-map]
                   (if (:random-insertion instruction-map)
                     (dissoc instruction-map :random-insertion)
                     (assoc instruction-map
                            :parent-uuid (:uuid instruction-map)
                            :uuid (java.util.UUID/randomUUID))))
                 genome))))

(defn perform-genetic-operator
  "Takes a single genetic operator keyword or a sequence of operator keywords,
   and performs them to create a new individual. Uses recursive helper function
   even with a single operator by putting that operator in a vector."
  [operator population location rand-gen 
   {:keys [max-points genome-representation
           track-instruction-maps] :as argmap}]
  (let [first-parent (if (= 0 (:parents (get genetic-operators operator)))
                       nil
                       (select population argmap))
        operator-vector (if (sequential? operator) operator (vector operator))
        child (perform-genetic-operator-list operator-vector first-parent
                                             population location rand-gen argmap)]
    (cond->
        (assoc child :genetic-operators operator)

      (> (count (:genome child))
         (* (/ max-points 4)
            (if (= genome-representation :plush)
              1
              plushy-max-genome-size-modifier)))
      (as-> c (revert-too-big-child first-parent c argmap))

      track-instruction-maps
      (update-instruction-map-uuids))))

(defn fibonacci
  "Used for Fibonacci age gap in ALPS"
  [n]
  (loop [n n
         x 0
         y 1]
    (if (<= n 0)
      y
      (recur (dec n)
             y
             (+' x y)))))

(defn age-limit-of-index
  "Gives the age limit of parents at this index when using ALPS"
  [index {:keys [ALPS-number-of-layers ALPS-age-limit-system
                 max-generations population-size]
          :as argmap}]
  (let [layer-size (int (quot population-size ALPS-number-of-layers))
        layer-num (inc (quot index layer-size))
        age-limit-fn (case ALPS-age-limit-system
                       :linear identity
                       :polynomial #(* % %)
                       :exponential #(int (Math/pow 2 (dec %)))
                       :fibonacci fibonacci)
        age-gap (/ max-generations (age-limit-fn ALPS-number-of-layers))]
    (* age-gap (age-limit-fn layer-num))))

(defn breed
  "Returns an individual bred from the given population using the given parameters."
  [agt ;necessary since breed is called using swap! or send, even though not used
   location rand-gen population
   {:keys [genetic-operator-probabilities use-ALPS ALPS-number-of-layers] :as argmap}]
  (random/with-rng rand-gen
    (let [pop (if use-ALPS ; We need this filtered population only if using ALPS
                (filter #(< (:age %)
                            (age-limit-of-index location argmap))
                        population)
                population)]
      (if (not (empty? pop))
        (let [prob (lrand)]
          (loop [vectored-go-probabilities (reductions #(assoc %2 1 (+ (second %1) (second %2)))
                                                       (vec genetic-operator-probabilities))]
            (if (or (= 1 (count vectored-go-probabilities))
                    (<= prob (second (first vectored-go-probabilities))))
              (perform-genetic-operator (first (first vectored-go-probabilities)) 
                                        pop location rand-gen argmap)
              (recur (rest vectored-go-probabilities)))))
        ; If false, then using ALPS and no individuals are in filtered pop. This
        ; means that everyone is too old for this layer, and we need a new individual.
        (genesis argmap)))))


