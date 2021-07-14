(ns clojush.pushgp.genetic-operators
  (:use [clojush util random individual globals interpreter translate pushstate]
        [clojush.instructions tag gtm]
        [clojush.pushgp.selection.selection]
        [clojure.math.numeric-tower])
  (:import (org.apache.commons.math3.stat.inference TTest))
  (:require [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local utilities

(defn random-element-or-identity-if-not-a-collection
  "If given a number, returns it. If given a collection, returns a member of the collection.
  Intended for allowing arguments to genetic operators, such as mutation rates, to take
  collections in addition to single values"
  [thing-or-collection]
  (if (coll? thing-or-collection)
    (lrand-nth thing-or-collection)
    thing-or-collection))

(declare produce-child-genome-by-autoconstruction) ;; forward declaration for grain size calculations

(defn age-combining-function
  "Returns the actual age combining function specified by the :age-combining-function
  in the argmap. The function will take three arguments: the two parents, and the
  genome of the child."
  [argmap]
  (case (:age-combining-function argmap)
    :max (fn [p1 p2 g] 
           (max (inc (:age p1)) 
                (inc (:age p2))))
    :min (fn [p1 p2 g] 
           (min (inc (:age p1)) 
                (inc (:age p2))))
    :average (fn [p1 p2 g] 
               (average (inc (:age p1)) 
                        (inc (:age p2))))
    :proportionate (fn [p1 p2 g]
                     (if (= (:age p1) (:age p2))
                       (inc (:age p1))
                       (if (= (:genome p1) (:genome p2))
                         (/ (+ (inc (:age p1)) (inc (:age p2))) 2)
                         (let [p1-dist (levenshtein-distance g (:genome p1))
                               p2-dist (levenshtein-distance g (:genome p2))]
                           (+ (inc (:age p1))
                              (* (/ p1-dist (+ p1-dist p2-dist))
                                 (- (inc (:age p2))
                                    (inc (:age p1)))))))))
    :first (fn [p1 p2 g]
             (inc (:age p1)))
    :half-progressive (fn [p1 p2 g]
                        (if (= (:age p1) (:age p2))
                          (inc (:age p1))
                          (if (= (:genome p1) (:genome p2))
                            (/ (+ (inc (:age p1)) (inc (:age p2))) 2)
                            (if (> (:age p2) (:age p1))
                              (inc (:age p1))
                              (let [p1-dist (levenshtein-distance g (:genome p1))
                                    p2-dist (levenshtein-distance g (:genome p2))]
                                (+ (inc (:age p1))
                                   (* (/ p1-dist (+ p1-dist p2-dist))
                                      (- (inc (:age p2))
                                         (inc (:age p1))))))))))
    :progressive (fn [p1 p2 g]
                   (if (= (:age p1) (:age p2))
                     (inc (:age p1))
                     (if (= (:genome p1) (:genome p2))
                       (/ (+ (inc (:age p1)) (inc (:age p2))) 2)
                       (let [p1-dist (levenshtein-distance g (:genome p1))
                             p2-dist (levenshtein-distance g (:genome p2))]
                         (+ (inc (min (:age p1) (:age p2)))
                            (* (Math/abs (float (- (:age p1) (:age p2))))
                            	         (/ (Math/abs (float (- p1-dist p2-dist)))
                                            (+ p1-dist p2-dist))))))))
    :first-reuse (fn [p1 p2 g]
                   (+ (:age p1)
                      (max (sequence-similarity g (:genome p1))
                           (sequence-similarity g (:genome p2)))))))

;; test effects of :proportionate with expressions like this:
;(float ((age-combining-function {:age-combining-function :proportionate})
;        {:genome [1 2 3 4 5 6 7] :age 100}
;        {:genome [1 2 3 4] :age 200}
;        [1 2 3]))

(defn compute-grain-size
  "Returns the grain size for the individual with the provided genome, as
  produced by the provided parents, according to the :random-screen settings
  in argmap. Grain sizes should range from 0 to 1. Individuals with smaller
  grain sizes will survive screens of a wider range of sizes."
  ([genome {:keys [random-screen] :as argmap}]
   (compute-grain-size genome 
                       {:genome [] :dummy true :age -1} 
                       {:genome [] :dummy true :age -1} 
                       argmap))
  ([genome parent1 {:keys [random-screen] :as argmap}]
   (compute-grain-size genome 
                       parent1 
                       {:genome [] :dummy true :age -1} 
                       argmap))
  ([genome parent1 parent2 {:keys [random-screen] :as argmap}]
   (if (or (not random-screen)
           (empty? genome))
     1
     (if (some :dummy [parent1 parent2])
       (if (some #{(:criterion random-screen)} 
                 #{:non-zero-genetic-similarity-to-parent})
         1
         0)
       (case (:criterion random-screen)
         ;
         :genetic-similarity-to-parent
         (max (sequence-similarity genome (:genome parent1))
              (sequence-similarity genome (:genome parent2)))
         :non-zero-genetic-similarity-to-parent
         (let [raw-similarity (max (sequence-similarity genome (:genome parent1))
                                   (sequence-similarity genome (:genome parent2)))]
           (if (zero? raw-similarity)
             1
             raw-similarity))
         ;
         :genetic-difference-from-parent
         (- 1 (max (sequence-similarity genome (:genome parent1))
                   (sequence-similarity genome (:genome parent2))))
         ;
         :reproductive-similarity-to-parent
         (sequence-similarity 
           genome
           (produce-child-genome-by-autoconstruction
             genome
             (:genome parent1)
             (:genome parent2)
             argmap))
         ;
         :reproductive-difference-from-parent
         (- 1 (sequence-similarity 
                genome
                (produce-child-genome-by-autoconstruction
                  genome
                  (:genome parent1)
                  (:genome parent2)
                  argmap))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reproduction

(defn reproduction
  "Returns parent.
  Works with Plushy genomes."
  [ind argmap]
  ind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genesis

(defn genesis
  "Ignores the provided parent and returns a new, random individual, with age 0.
  Works with Plushy genomes."
  [{:keys [max-genome-size-in-initial-program atom-generators genome-representation]
    :as argmap}]
  (let [genome (case genome-representation
                 :plush (random-plush-genome max-genome-size-in-initial-program
                                             atom-generators
                                             argmap)
                 :plushy (random-plushy-genome
                          (* plushy-max-genome-size-modifier
                             max-genome-size-in-initial-program)
                          atom-generators
                          argmap))]
    (make-individual :genome genome
                     :history ()
                     :age 0
                     :genetic-operators :random
                     :grain-size (compute-grain-size genome argmap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform mutation

(defn gaussian-noise-factor
  "Returns gaussian noise of mean 0, std dev 1."
  []
  (*' (Math/sqrt (*' -2.0 (Math/log (lrand))))
      (Math/cos (*' 2.0 Math/PI (lrand)))))

(defn perturb-with-gaussian-noise
  "Returns n perturbed with std dev sd."
  [sd n]
  (+' n (*' sd (gaussian-noise-factor))))

(defn tag-gaussian-tweak
  "Tweaks the tag with Gaussian noise."
  [instr uniform-mutation-tag-gaussian-standard-deviation]
  (let [tagparts (string/split (name instr) #"_")
        tag-num (read-string (last tagparts))
        new-tag-num (mod (round (perturb-with-gaussian-noise 
                                  uniform-mutation-tag-gaussian-standard-deviation tag-num))
                         @global-tag-limit)
        new-instr (symbol (apply str (interpose "_" (concat (butlast tagparts) 
                                                            (list (str new-tag-num))))))]
    new-instr))

(defn uniform-mutation
  "Uniformly mutates individual. For each token in the genome, there is
   uniform-mutation-rate probability of being mutated. If a token is to be
   mutated, it has a uniform-mutation-constant-tweak-rate probability of being
   mutated using a constant mutator (which varies depending on the type of the
   token), and otherwise is replaced with a random instruction.
   Works with Plushy genomes."
  [ind {:keys [uniform-mutation-rate uniform-mutation-constant-tweak-rate
               uniform-mutation-float-gaussian-standard-deviation
               uniform-mutation-int-gaussian-standard-deviation
               uniform-mutation-tag-gaussian-standard-deviation
               uniform-mutation-string-char-change-rate maintain-ancestors
               atom-generators genome-representation]
        :as argmap}]
  (let [uniform-mutation-rate 
        (random-element-or-identity-if-not-a-collection uniform-mutation-rate)
        
        uniform-mutation-constant-tweak-rate 
        (random-element-or-identity-if-not-a-collection uniform-mutation-constant-tweak-rate)
        
        uniform-mutation-float-gaussian-standard-deviation 
        (random-element-or-identity-if-not-a-collection uniform-mutation-float-gaussian-standard-deviation)
        
        uniform-mutation-int-gaussian-standard-deviation 
        (random-element-or-identity-if-not-a-collection uniform-mutation-int-gaussian-standard-deviation)
        
        uniform-mutation-tag-gaussian-standard-deviation 
        (random-element-or-identity-if-not-a-collection uniform-mutation-tag-gaussian-standard-deviation)
        
        uniform-mutation-string-char-change-rate 
        (random-element-or-identity-if-not-a-collection uniform-mutation-string-char-change-rate)
    
        string-tweak (fn [st]
                       (apply str (map (fn [c]
                                         (if (< (lrand) uniform-mutation-string-char-change-rate)
                                           (lrand-nth (vec (concat ["\n" "\t"] 
                                                                   (map (comp str char) 
                                                                        (range 32 127)))))
                                           c))
                                       st)))
        instruction-mutator (fn [token]
                              (case genome-representation
                                     :plush (assoc token
                                                   :instruction
                                                   (:instruction
                                                    (random-plush-instruction-map atom-generators argmap)))
                                     :plushy (random-plushy-instruction atom-generators argmap)))
        constant-mutator (fn [token]
                           (let [const (case genome-representation
                                         :plush (:instruction token)
                                         :plushy token)
                                 new-const
                                 (cond
                                   ;; tag
                                   (tag-instruction? const)
                                   (tag-gaussian-tweak const
                                                       uniform-mutation-tag-gaussian-standard-deviation)
                                   ;; float
                                   (float? const) 
                                   (perturb-with-gaussian-noise 
                                    uniform-mutation-float-gaussian-standard-deviation const)
                                   ;; integer
                                   (integer? const) 
                                   (round (perturb-with-gaussian-noise 
                                           uniform-mutation-int-gaussian-standard-deviation const))
                                   ;; string
                                   (string? const) 
                                   (string-tweak const)
                                   ;; boolean
                                   (or (= const true) (= const false)) 
                                   (lrand-nth [true false])
                                   ;; anything else
                                   :else
                                   (case genome-representation
                                     :plush (:instruction 
                                             (random-plush-instruction-map atom-generators argmap))
                                     :plushy (random-plushy-instruction atom-generators argmap)))]
                             (case genome-representation
                               :plush (assoc token :instruction new-const)
                               :plushy new-const)))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-rate)
                          (if (< (lrand) uniform-mutation-constant-tweak-rate)
                            (constant-mutator token)
                            (instruction-mutator token))
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform instruction mutation

(defn uniform-instruction-mutation
  "Uniformly mutates individual. For each token in the genome, there is
   uniform-mutation-rate probability of being mutated. If a token is to be
   mutated it will be replaced with a random instruction."
  [ind {:keys [uniform-mutation-rate maintain-ancestors atom-generators]
        :as argmap}]
  (let [uniform-mutation-rate (random-element-or-identity-if-not-a-collection uniform-mutation-rate)
        instruction-mutator (fn [token]
                              (assoc token
                                :instruction
                                (:instruction 
                                  (first (random-plush-genome 1 atom-generators argmap)))))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-rate)
                          (instruction-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform integer mutation

(defn uniform-integer-mutation
  "Uniformly mutates individual. For each integer in the genome, there is
   uniform-mutation-constant-tweak-rate probability of being mutated."
  [ind {:keys [uniform-mutation-constant-tweak-rate 
               uniform-mutation-int-gaussian-standard-deviation
               maintain-ancestors atom-generators]
        :as argmap}]
  (let [uniform-mutation-constant-tweak-rate 
        (random-element-or-identity-if-not-a-collection uniform-mutation-constant-tweak-rate)
        
        uniform-mutation-int-gaussian-standard-deviation 
        (random-element-or-identity-if-not-a-collection uniform-mutation-int-gaussian-standard-deviation)
        
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (integer? const)
                               (assoc token
                                 :instruction
                                 (round (perturb-with-gaussian-noise 
                                          uniform-mutation-int-gaussian-standard-deviation 
                                          const)))
                               token)))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-constant-tweak-rate)
                          (constant-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform float mutation

(defn uniform-float-mutation
  "Uniformly mutates individual. For each float in the genome, there is
   uniform-mutation-constant-tweak-rate probability of being mutated."
  [ind {:keys [uniform-mutation-constant-tweak-rate 
               uniform-mutation-float-gaussian-standard-deviation
               maintain-ancestors atom-generators]
        :as argmap}]
  (let [uniform-mutation-constant-tweak-rate 
        (random-element-or-identity-if-not-a-collection uniform-mutation-constant-tweak-rate)
        
        uniform-mutation-float-gaussian-standard-deviation
        (random-element-or-identity-if-not-a-collection uniform-mutation-float-gaussian-standard-deviation)
        
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (float? const)
                               (assoc token
                                 :instruction
                                 (perturb-with-gaussian-noise 
                                   uniform-mutation-float-gaussian-standard-deviation const))
                               token)))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-constant-tweak-rate)
                          (constant-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform tag mutation

(defn uniform-tag-mutation
  "Uniformly mutates individual. For each tag instruction in the genome, there is
   uniform-mutation-rate probability of being mutated.
   Works with Plushy genomes."
  [ind {:keys [uniform-mutation-rate uniform-mutation-tag-gaussian-standard-deviation
               maintain-ancestors atom-generators genome-representation]
        :as argmap}]
  (let [uniform-mutation-rate 
        (random-element-or-identity-if-not-a-collection uniform-mutation-rate)
        
        uniform-mutation-tag-gaussian-standard-deviation
        (random-element-or-identity-if-not-a-collection uniform-mutation-tag-gaussian-standard-deviation)
         
        constant-mutator (fn [token]
                           (let [const (case genome-representation
                                         :plush (:instruction token)
                                         :plushy token)]
                             (if (tag-instruction? const)
                               (case genome-representation
                                 :plush (assoc token :instruction (tag-gaussian-tweak const
                                                                                      uniform-mutation-tag-gaussian-standard-deviation))
                                 :plushy (tag-gaussian-tweak const
                                                             uniform-mutation-tag-gaussian-standard-deviation))
                               token)))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-rate)
                          (constant-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform string mutation

(defn uniform-string-mutation
  "Uniformly mutates individual. For each string literal in the genome, there is
   uniform-mutation-rate probability of being mutated."
  [ind {:keys [uniform-mutation-rate uniform-mutation-string-char-change-rate
               maintain-ancestors atom-generators]
        :as argmap}]
  (let [uniform-mutation-rate 
        (random-element-or-identity-if-not-a-collection uniform-mutation-rate)
        
        uniform-mutation-string-char-change-rate
        (random-element-or-identity-if-not-a-collection uniform-mutation-string-char-change-rate)
        
        string-tweak (fn [st]
                       (apply str (map (fn [c]
                                         (if (< (lrand) uniform-mutation-string-char-change-rate)
                                           (lrand-nth (vec (concat ["\n" "\t"] 
                                                                   (map (comp str char) 
                                                                        (range 32 127)))))
                                           c))
                                       st)))
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (string? const) 
                               (assoc token :instruction (string-tweak const))
                               token)))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-rate)
                          (constant-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform boolean mutation

(defn uniform-boolean-mutation
  "Uniformly mutates individual. For each boolean in the genome, there is
   uniform-mutation-constant-tweak-rate probability of being mutated."
  [ind {:keys [uniform-mutation-constant-tweak-rate 
               maintain-ancestors atom-generators]
        :as argmap}]
  (let [uniform-mutation-constant-tweak-rate (random-element-or-identity-if-not-a-collection uniform-mutation-constant-tweak-rate)
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (or (= const true) (= const false)) 
                               (assoc token :instruction (not const))
                               token)))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-constant-tweak-rate)
                          (constant-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform close mutation

(defn uniform-close-mutation
  "Uniformly mutates the :close's in the individual's instruction maps. Each
   :close will have a uniform-close-mutation-rate probability of being changed,
   and those that are changed have a close-increment-rate chance of being
   incremented, and are otherwise decremented."
  [ind {:keys [uniform-close-mutation-rate close-increment-rate
               epigenetic-markers maintain-ancestors]
        :as argmap}]
  (if (not (some #{:close} epigenetic-markers))
    ind
    (let [uniform-close-mutation-rate (random-element-or-identity-if-not-a-collection uniform-close-mutation-rate)
          close-increment-rate (random-element-or-identity-if-not-a-collection close-increment-rate)
          close-mutator (fn [instr-map]
                          (let [closes (get instr-map :close 0)]
                            (assoc instr-map :close
                              (if (< (lrand) uniform-close-mutation-rate)
                                (if (< (lrand) close-increment-rate)
                                  ;Rate at which to increase closes instead of decrease
                                  (inc closes)
                                  (if (<= closes 0)
                                    0
                                    (dec closes)))
                                closes))))
          new-genome (mapv close-mutator (:genome ind))]
      (make-individual :genome new-genome
                       :history (:history ind)
                       :grain-size (compute-grain-size new-genome ind argmap)
                       :ancestors (if maintain-ancestors
                                    (cons (:genome ind) (:ancestors ind))
                                    (:ancestors ind))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform silence mutation

(defn uniform-silence-mutation
  "Uniformly mutates the :silent's in the individual's instruction maps. Each
   :silent will have a uniform-silence-mutation-rate probability of being switched."
  [ind {:keys [uniform-silence-mutation-rate
               epigenetic-markers maintain-ancestors]
        :as argmap}]
  (if (not (some #{:silent} epigenetic-markers))
    ind
    (let [uniform-silence-mutation-rate (random-element-or-identity-if-not-a-collection uniform-silence-mutation-rate)
          silent-mutator (fn [instr-map]
                           (let [silent (get instr-map :silent false)]
                             (assoc instr-map :silent
                                    (if (< (lrand) uniform-silence-mutation-rate)
                                      (not silent)
                                      silent))))
          new-genome (mapv silent-mutator (:genome ind))]
      (make-individual :genome new-genome
                       :history (:history ind)
                       :grain-size (compute-grain-size new-genome ind argmap)
                       :ancestors (if maintain-ancestors
                                    (cons (:genome ind) (:ancestors ind))
                                    (:ancestors ind))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform deletion

(defn uniform-deletion
  "Returns the individual with each element of its genome possibly deleted, with probability
given by uniform-deletion-rate.
  Works with Plushy genomes."
  [ind {:keys [uniform-deletion-rate maintain-ancestors] :as argmap}]
  (let [rate (random-element-or-identity-if-not-a-collection uniform-deletion-rate)
        new-genome (vec (filter identity
                                (mapv #(if (< (lrand) rate) nil %)
                                      (:genome ind))))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform addition

(defn uniform-addition
  "Returns the individual with each element of its genome possibly preceded or followed by
  a new gene, with probability given by uniform-addition-rate.
  Works with Plushy genomes."
  [ind {:keys [uniform-addition-rate maintain-ancestors atom-generators] :as argmap}]
  (let [rate (random-element-or-identity-if-not-a-collection uniform-addition-rate)
        new-genome (vec (apply concat
                               (mapv #(if (< (lrand) rate)
                                        (lshuffle [% 
                                                   (random-genome-gene
                                                     atom-generators argmap)])
                                        [%])
                                     (:genome ind))))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform addition and deletion (UMAD)

(defn uniform-addition-and-deletion
  "Returns the individual after two passes of mutation. In the first pass, each element of 
  its genome may possibly be preceded or followed by a new gene. In the second pass, each
  element of the genome may possibly be deleted. Probabilities are given by 
  uniform-addition-and-deletion-rate.
  Works with Plushy genomes."
  [ind {:keys [uniform-addition-and-deletion-rate maintain-ancestors atom-generators] 
        :as argmap}]
  (let [addition-rate (random-element-or-identity-if-not-a-collection uniform-addition-and-deletion-rate)
        deletion-rate (if (zero? addition-rate)
                        0
                        (/ 1 (+ (/ 1 addition-rate) 1)))
        after-addition (vec (apply concat
                                   (mapv #(if (< (lrand) addition-rate)
                                            (lshuffle [% 
                                                       (random-genome-gene
                                                         atom-generators argmap)])
                                            [%])
                                         (:genome ind))))
        new-genome (vec (remove (fn [_] (< (lrand) deletion-rate))
                                after-addition))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform combination

(defn uniform-combination
  "Returns child genome created through crossover of two parents. Each gene in parent1
  is considered in turn, and depending on some probability, may be preceded or followed
  by the corresponding element from parent2 (which will wrap if it is too short).
  Probability is given by uniform-combination-rate.
  Works with Plushy genomes."
  [parent1 parent2
   {:keys [uniform-combination-rate maintain-ancestors]
    :as argmap}]
  (let [combination-rate (random-element-or-identity-if-not-a-collection uniform-combination-rate)
        new-genome (vec
                    (apply concat
                           (mapv (fn [g1 g2]
                                   (if (< (lrand) combination-rate)
                                     (lshuffle [g1 g2])
                                     [g1]))
                                 (:genome parent1)
                                 (cycle (:genome parent2)))))]
    (make-individual :genome new-genome
                     :history (:history parent1)
                     :grain-size (compute-grain-size new-genome parent1 parent2 argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome parent1) (:ancestors parent1))
                                  (:ancestors parent1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform combination and deletion

(defn uniform-combination-and-deletion
  "Returns parent1 after two passes of modification. In the first pass, each element 
  of its genome may possibly be preceded or followed by the corresponding element from
  parent 2's genome (which will wrap if it is too short). In the second pass, each
  element of the genome may possibly be deleted. Probabilities are given by 
  uniform-combination-and-deletion-rate.
  Works with Plushy genomes."
  [parent1 parent2 
   {:keys [uniform-combination-and-deletion-rate maintain-ancestors]
    :as argmap}]
  (let [combination-rate (random-element-or-identity-if-not-a-collection uniform-combination-and-deletion-rate)
        deletion-rate (if (zero? combination-rate)
                        0
                        (/ 1 (+ (/ 1 combination-rate) 1)))
        after-combination (vec 
                            (apply concat
                                   (mapv (fn [g1 g2]
                                           (if (< (lrand) combination-rate)
                                             (lshuffle [g1 g2])
                                             [g1]))
                                         (:genome parent1)
                                         (cycle (:genome parent2)))))
        new-genome (vec (filter identity
                                (mapv #(if (< (lrand) deletion-rate) nil %)
                                      after-combination)))]
    (make-individual :genome new-genome
                     :history (:history parent1)
                     :grain-size (compute-grain-size new-genome parent1 parent2 argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome parent1) (:ancestors parent1))
                                  (:ancestors parent1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alternation

(defn alternation
  "Uniformly alternates between the two parents using a similar method to that
   used in ULTRA.
  Works with Plushy genomes."
  [parent1 parent2 {:keys [alternation-rate alignment-deviation
                           max-points maintain-ancestors] :as argmap}]
  (let [alternation-rate (random-element-or-identity-if-not-a-collection alternation-rate)
        alignment-deviation (random-element-or-identity-if-not-a-collection alignment-deviation)
        s1 (:genome parent1)
        s2 (:genome parent2)
        new-genome (loop [i 0
                          use-s1 (lrand-nth [true false])
                          result-genome []
                          iteration-budget (+ (count s1) (count s2))]
                     (if (or (>= i (count (if use-s1 s1 s2))) ;; finished current program
                             (> (count result-genome) (/ max-points 4)) ;; runaway growth
                             (<= iteration-budget 0)) ;; looping too long
                       result-genome ;; Return
                       (if (< (lrand) alternation-rate)
                         (recur (max 0 (+' i (Math/round (*' alignment-deviation 
                                                             (gaussian-noise-factor)))))
                                (not use-s1)
                                result-genome
                                (dec iteration-budget))
                         (recur (inc i)
                                use-s1
                                (conj result-genome (nth (if use-s1 s1 s2) i))
                                (dec iteration-budget)))))]
    (make-individual :genome new-genome
                     :history (:history parent1)
                     :grain-size (compute-grain-size new-genome parent1 parent2 argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome parent1) (:ancestors parent1))
                                  (:ancestors parent1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; two-point crossover
(defn two-point-crossover
  "Crossover of two parents. Each parent will have two points randomly selected, and
   the code between the two points in the first parent will be replaced by the
   code between the two points in the second parent.
   Works with Plushy genomes."
  [parent1 parent2 {:keys [maintain-ancestors] :as argmap}]
  (let [genome1 (:genome parent1)
        genome2 (:genome parent2)
        p1-points (sort (repeatedly 2 #(lrand-int (inc (count genome1)))))
        p2-points (sort (repeatedly 2 #(lrand-int (inc (count genome2)))))
        new-genome (vec (concat (take (first p1-points) 
                                      genome1)
                                (drop (first p2-points) 
                                      (take (second p2-points) genome2))
                                (drop (second p1-points) 
                                      genome1)))]
    (make-individual :genome new-genome
                     :history (:history parent1)
                     :grain-size (compute-grain-size new-genome parent1 parent2 argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome parent1) (:ancestors parent1))
                                  (:ancestors parent1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; traditional uniform crossover from GAs

(defn remove-uniform-padding
  "Removes instances of 'uniform-padding from genome."
  [genome]
  (remove #{'uniform-padding} genome))

(defn uniform-crossover
  "Uniform crossover of two parents. At each index in the child, an instruction
   will be taken from one of the two parents at random.
   Works with Plushy genomes."
  [parent1 parent2 {:keys [maintain-ancestors] :as argmap}]
  (if (> (count (:genome parent1)) (count (:genome parent2)))
    (recur parent2 parent1 argmap)
    (let [short-genome (:genome parent1)
          long-genome (:genome parent2)
          short-genome-lengthened (vec (concat short-genome
                                               (repeat (- (count long-genome) 
                                                          (count short-genome))
                                                       'uniform-padding)))
          new-genome (vec (remove-uniform-padding
                            (mapv (fn [x1 x2]
                                    (if (< (lrand) 0.5)
                                      x1
                                      x2))
                                  short-genome-lengthened
                                  long-genome)))]
      (make-individual :genome new-genome
                       :history (:history parent1)
                       :grain-size (compute-grain-size new-genome parent1 parent2 argmap)
                       :ancestors (if maintain-ancestors
                                    (cons (:genome parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gene-selection

(defn gene-selection
  "Takes each gene from a selected parent, with re-selection for each gene with probability
  abs(gene-selection-rate). Negative values for gene-selection-rate mean that indexing will
  run backwards from the ends of programs."
  [initial-parent {:keys [gene-selection-rate population maintain-ancestors] :as argmap}]
  (let [rate (random-element-or-identity-if-not-a-collection gene-selection-rate)
        new-genome (loop [parent-genome (:genome initial-parent)
                          index 0
                          child-genome []]
                     (if (> (inc index) (count parent-genome))
                       (if (pos? rate) 
                         child-genome
                         (reverse child-genome))
                       (recur (if (>= (Math/abs (float rate)) (lrand))
                                (:genome (select population argmap))
                                parent-genome)
                              (inc index)
                              (conj child-genome 
                                    (nth parent-genome 
                                         (if (pos? rate)
                                           index
                                           (- (count parent-genome) (inc index))))))))]
    (make-individual :genome new-genome
                     :history (:history initial-parent)
                     :grain-size (compute-grain-size new-genome initial-parent argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome initial-parent) (:ancestors initial-parent))
                                  (:ancestors initial-parent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform reordering

(defn uniform-reordering
  "Returns the individual with each pair of adjacent genes possibly reordered. For each call,
  pairs will randomly start with either the odd-indexed or even-indexed genes. The probability
  that a pair will be reordered is given by uniform-reordering-rate.
  Works with Plushy genomes."
  [ind {:keys [uniform-reordering-rate maintain-ancestors]
        :as argmap}]
  (let [partitioned (partition 2 (concat (if (zero? (rand-int 2)) [] [nil])
                                         (:genome ind)
                                         [nil]))
        rate (random-element-or-identity-if-not-a-collection uniform-reordering-rate)
        reordered (map #(if (< (rand) rate) (reverse %) %) partitioned)
        new-genome (vec (filter identity (apply concat reordered)))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform segment reordering

(defn uniform-segment-reordering
  "Returns the individual with each pair of adjacent segments possibly reordered. For each 
  call pairs will randomly start with either the odd-indexed or even-indexed segments. The
  probability that a pair will be reordered is given by uniform-reordering-rate. The
  probability that segmenting will occur at each gene is given by uniform-segmenting-rate. 
  Works with Plushy genomes."
  [ind {:keys [uniform-reordering-rate uniform-segmenting-rate maintain-ancestors]
        :as argmap}]
  (let [segmented (partition-by (fn [_]
                                  (< (rand)
                                     (random-element-or-identity-if-not-a-collection
                                      uniform-segmenting-rate)))
                                (:genome ind))
        partitioned (partition 2 (concat (if (zero? (rand-int 2)) [] [nil])
                                         segmented
                                         [nil]))
        rate (random-element-or-identity-if-not-a-collection uniform-reordering-rate)
        reordered (map #(if (< (rand) rate) (reverse %) %) partitioned)
        new-genome (vec (apply concat (filter identity (apply concat reordered))))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform segment transposition

(defn uniform-segment-duplication
  "Returns the individual with each segment possibly duplicated. The probability that a
  segment will be duplicated is given by uniform-duplication-rate. The probability that 
  segmenting will occur at each gene is given by uniform-segmenting-rate. 
  Works with Plushy genomes."
  [ind {:keys [uniform-segment-duplication-rate uniform-segmenting-rate maintain-ancestors]
        :as argmap}]
  (let [segmented (partition-by (fn [_]
                                  (< (rand)
                                     (random-element-or-identity-if-not-a-collection
                                      uniform-segmenting-rate)))
                                (:genome ind))
        rate (random-element-or-identity-if-not-a-collection uniform-segment-duplication-rate)
        new-genome (vec (apply concat (map #(if (< (rand) rate) (concat % %) %) 
                                           segmented)))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform segment deletion

(defn uniform-segment-deletion
  "Returns the individual with each segment possibly deleted. The probability that a
  segment will be deleted is given by uniform-deletion-rate. The probability that 
  segmenting will occur at each gene is given by uniform-segmenting-rate. 
  Works with Plushy genomes."
  [ind {:keys [uniform-segment-deletion-rate uniform-segmenting-rate maintain-ancestors]
        :as argmap}]
  (let [segmented (partition-by (fn [_]
                                  (< (rand)
                                     (random-element-or-identity-if-not-a-collection
                                      uniform-segmenting-rate)))
                                (:genome ind))
        rate (random-element-or-identity-if-not-a-collection uniform-segment-deletion-rate)
        new-genome (vec (apply concat (map #(if (< (rand) rate) [] %)
                                           segmented)))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform segment transposition

(defn uniform-segment-transposition
  "Returns the individual with each segment possibly transposed. The probability that a
  segment will be transposed is given by uniform-transposition-rate. The probability that 
  segmenting will occur at each gene is given by uniform-segmenting-rate. 
  Works with Plushy genomes."
  [ind {:keys [uniform-transposition-rate uniform-segmenting-rate maintain-ancestors]
        :as argmap}]
  (let [segmented (partition-by (fn [_]
                                  (< (rand)
                                     (random-element-or-identity-if-not-a-collection
                                      uniform-segmenting-rate)))
                                (:genome ind))
        rate (random-element-or-identity-if-not-a-collection uniform-transposition-rate)
        new-genome (vec (apply concat (map #(if (< (rand) rate) (reverse %) %)
                                           segmented)))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoconstuction

;; NOTE: EXPERIMENTAL!

(defn process-genome-for-autoconstruction
  "Replaces input instructions with noops and  autoconstructive_<type>_rand
  with <type>_rand."
  [genome]
  (mapv (fn [instruction-map]
          (cond 
            (= (:instruction instruction-map) 'autoconstructive_integer_rand)
            (assoc instruction-map :instruction 'integer_rand)
            ;
            (= (:instruction instruction-map) 'autoconstructive_boolean_rand)
            (assoc instruction-map :instruction 'boolean_rand)
            ;
            (= (:instruction instruction-map) 'autoconstructive_code_rand_atom)
            (assoc instruction-map :instruction 'code_rand_atom)
            ;
            :else
            instruction-map))
        genome))

(defn produce-child-genome-by-autoconstruction
  "Runs the program expressed by parent1-genome with both parent genomes
on the genome stack and also available via input instructions, and returns
the resulting top genome."
  ([parent1-genome parent2-genome argmap]
   (produce-child-genome-by-autoconstruction 
     parent1-genome parent1-genome parent2-genome argmap))
  ([genome-to-run parent1-genome parent2-genome argmap]
   (let [parent1-genome (with-meta parent1-genome {})
         parent2-genome (with-meta parent2-genome {})
         run-result (let [program-to-run 
                          (translate-plush-genome-to-push-program
                            {:genome
                             (process-genome-for-autoconstruction genome-to-run)}
                            argmap)]
                      (if (= (:autoconstructive-genome-instructions argmap) :gtm)
                        (let [run-pgm #(run-push program-to-run %)
                              after-gtm (-> (make-push-state)
                                            (init-gtm)
                                            (load-track 1 parent1-genome)
                                            (load-track 2 parent2-genome)
                                            (assoc :autoconstructing true)
                                            (run-pgm))]
                            (with-meta (dump-track after-gtm 0)
                              {:made-by (:trace (:gtm after-gtm))}))
                        (top-item :genome
                                  (run-push
                                    program-to-run
                                    (-> (if (= (:autoconstructive-genome-instructions argmap) :appending)
                                          (make-push-state)
                                          (->> (make-push-state)
                                               (push-item parent2-genome :genome)
                                               (push-item parent1-genome :genome)))
                                        (assoc :parent1-genome parent1-genome)
                                        (assoc :parent2-genome parent2-genome)
                                        (assoc :autoconstructing true))))))]
     (if (or (seq? run-result) (vector? run-result))
       (with-meta (vec run-result) (meta run-result))
       []))))

(defn expressed-program-sequence-from-genome
  "Returns an open-close sequenc for the program produced by expressing
genome g."
  [g argmap]
  (ensure-list
    (list-to-open-close-sequence
      (translate-plush-genome-to-push-program {:genome g} argmap))))

(defn expressed-difference
  "Returns the levenshtein distance between the open-close sequences for the
programs encoded by genomes g1 and g2."
  [g1 g2 argmap]
  (levenshtein-distance (expressed-program-sequence-from-genome g1 argmap)
                        (expressed-program-sequence-from-genome g2 argmap)))

(defn gecco2016-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        delta #(expressed-difference 
                 g
                 (produce-child-genome-by-autoconstruction g g argmap)
                 argmap)
        diffs (repeatedly 2 delta)]
    (assoc ind :diversifying
      (and (> (reduce min diffs) 0) ;; diversification threshold set here
           (> (count (distinct diffs)) 1)))))

(defn gecco2016-plus1-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        delta #(expressed-difference 
                 g
                 (produce-child-genome-by-autoconstruction g g argmap)
                 argmap)
        diffs (repeatedly 3 delta)]
    (assoc ind :diversifying
      (and (> (reduce min diffs) 0) ;; diversification threshold set here
           (> (count (distinct diffs)) 2)))))

(defn gecco2016-plus2-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        delta #(expressed-difference 
                 g
                 (produce-child-genome-by-autoconstruction g g argmap)
                 argmap)
        diffs (repeatedly 4 delta)]
    (assoc ind :diversifying
      (and (> (reduce min diffs) 0) ;; diversification threshold set here
           (> (count (distinct diffs)) 3)))))

(defn doesnt-clone-diversifying?
  [ind argmap]
  (let [g (:genome ind)]
    (assoc ind :diversifying
      (not= (translate-plush-genome-to-push-program 
              {:genome g} 
              argmap)
            (translate-plush-genome-to-push-program 
              {:genome (produce-child-genome-by-autoconstruction g g argmap)} 
              argmap)))))

(defn doesnt-clone-genetically-diversifying?
  [ind argmap]
  (let [g (:genome ind)]
    (assoc ind :diversifying
      (not= g (produce-child-genome-by-autoconstruction g g argmap)))))

(defn child-doesnt-clone-diversifying?
  [ind argmap]
  (let [g (:genome ind)]
    (let [child-genome (produce-child-genome-by-autoconstruction g g argmap)]
      (assoc ind :diversifying
        (not= (translate-plush-genome-to-push-program 
                {:genome child-genome} 
                argmap)
              (translate-plush-genome-to-push-program 
                {:genome (produce-child-genome-by-autoconstruction 
                           child-genome child-genome argmap)} 
                argmap))))))

(defn not-a-clone-diversifying?
  [ind {:keys [parent1-genome parent2-genome] :as argmap}]
  (let [g (:genome ind)
        express #(translate-plush-genome-to-push-program {:genome %} argmap)
        pgm (express g)
        parent1-pgm (express parent1-genome)
        parent2-pgm (express parent2-genome)]
    (assoc ind :diversifying
           (and (not= pgm parent1-pgm)
                (not= pgm parent2-pgm)))))

(defn lineage-behavior-diversifying?
  [ind argmap]
  (if (not (:print-history argmap))
    (throw
     (Exception.
      ":print-history must be true for :lineage-behavior diversification test"))
    (assoc ind :diversifying
           (let [hist (:parent1-history argmap)]
             (or (< (count hist) 2)
                 (apply distinct? hist))))))

(defn new-errors-diversifying?
  [ind argmap]
  (if (not (:print-history argmap))
    (throw
     (Exception.
      ":print-history must be true for :new-errors diversification test"))
    (let [errs (or (:errors ind)
                   (do
                     (swap! evaluations-count inc)
                     (:errors ((:error-function argmap)
                               {:genome (:genome ind)
                                :program (translate-plush-genome-to-push-program
                                          {:genome (:genome ind)}
                                          argmap)}))))]
      (assoc ind :diversifying
             (not (some #{errs} (:parent1-history argmap)))))))

(defn at-least-half-new-errors-diversifying?
  [ind argmap]
  (if (not (:print-history argmap))
    (throw
     (Exception.
      ":print-history must be true for :at-least-half-new-errors diversification test"))
    (let [errs (or (:errors ind)
                   (do
                     (swap! evaluations-count inc)
                     (:errors ((:error-function argmap)
                               {:genome (:genome ind)
                                :program (translate-plush-genome-to-push-program
                                          {:genome (:genome ind)}
                                          argmap)}))))]
      (assoc ind :diversifying
             (let [hist (:parent1-history argmap)]
               (>= (* 2 (count (distinct (conj hist errs))))
                   (count (conj hist errs))))))))

(defn enough-new-errors-diversifying?
  [ind argmap]
  (if (not (:print-history argmap))
    (throw
     (Exception.
      ":print-history must be true for :enough-new-errors diversification test"))
    (let [errs (or (:errors ind)
                   (do
                     (swap! evaluations-count inc)
                     (:errors ((:error-function argmap)
                               {:genome (:genome ind)
                                :program (translate-plush-genome-to-push-program
                                          {:genome (:genome ind)}
                                          argmap)}))))]
      (assoc ind :diversifying
             (let [hist (:parent1-history argmap)]
               (>= (count (distinct (conj hist errs)))
                   (* (:autoconstructive-enough-new-errors-fraction argmap)
                      (count (conj hist errs)))))))))

(defn not-empty-diversifying?
  [ind argmap]
  (assoc ind :diversifying
         (not (empty? (translate-plush-genome-to-push-program ind argmap)))))

(defn minimum-genetic-difference-diversifying?
  [ind {:keys [parent1-genome parent2-genome]}]
  (let [g (:genome ind)]
    (assoc ind :diversifying
           (and (<= (sequence-similarity g parent1-genome) 0.9)
                (<= (sequence-similarity g parent2-genome) 0.9)))))

(defn three-gens-diff-diffs-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        make-child #(produce-child-genome-by-autoconstruction % % argmap)
        diff #(expressed-difference %1 %2 argmap)
        c1 (make-child g)
        c2 (make-child g)
        gc1 (make-child c1)
        gc2 (make-child c2)
        c1-diff (diff g c1)
        c2-diff (diff g c2)
        gc1-diff (diff c1 gc1)
        gc2-diff (diff c2 gc2)
        diffs [c1-diff c2-diff gc1-diff gc2-diff]]
    (assoc ind :diversifying
      (and (> (reduce min diffs) 0)
           (apply distinct? diffs)))))

(defn three-gens-same-inputs-diff-diffs-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        make-child #(produce-child-genome-by-autoconstruction % g g argmap)
        diff #(expressed-difference %1 %2 argmap)
        c1 (make-child g)
        c2 (make-child g)
        gc1 (make-child c1)
        gc2 (make-child c2)
        c1-diff (diff g c1)
        c2-diff (diff g c2)
        gc1-diff (diff c1 gc1)
        gc2-diff (diff c2 gc2)
        diffs [c1-diff c2-diff gc1-diff gc2-diff]]
    (assoc ind :diversifying
      (and (> (reduce min diffs) 0)
           (apply distinct? diffs)))))

(defn four-gens-same-inputs-diff-diffs-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        make-child #(produce-child-genome-by-autoconstruction % g g argmap)
        diff #(expressed-difference %1 %2 argmap)
        c1 (make-child g)
        c2 (make-child g)
        gc1 (make-child c1)
        gc2 (make-child c2)
        ggc1 (make-child gc1)
        ggc2 (make-child gc2)
        c1-diff (diff g c1)
        c2-diff (diff g c2)
        gc1-diff (diff c1 gc1)
        gc2-diff (diff c2 gc2)
        ggc1-diff (diff gc1 ggc1)
        ggc2-diff (diff gc2 ggc2)
        diffs [c1-diff c2-diff gc1-diff gc2-diff ggc1-diff ggc2-diff]]
    (assoc ind :diversifying
      (and (> (reduce min diffs) 0)
           (apply distinct? diffs)))))

#_(defn two-x-two-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        make-child #(produce-child-genome-by-autoconstruction % g g argmap)
        diff #(expressed-difference %1 %2 argmap)
        c1 (make-child g)
        c2 (make-child g)
        gc1a (make-child c1)
        gc1b (make-child c1)
        gc2a (make-child c2)
        gc2b (make-child c2)
        c1-diff (diff g c1)
        c2-diff (diff g c2)
        gc1a-diff (diff c1 gc1a)
        gc1b-diff (diff c1 gc1b)
        gc2a-diff (diff c2 gc2a)
        gc2b-diff (diff c2 gc2b)
        diffs [c1-diff c2-diff gc1a-diff gc1b-diff gc2a-diff gc2b-diff]]
    (assoc ind :diversifying
      (and (> (reduce min diffs) 0)
           (apply distinct? diffs)))))

(defn two-x-two-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        make-child #(produce-child-genome-by-autoconstruction % g g argmap)
        diff #(expressed-difference %1 %2 argmap)
        c1 (make-child g)
        c2 (make-child g)
        gc1a (make-child c1)
        gc1b (make-child c1)
        gc2a (make-child c2)
        gc2b (make-child c2)
        gc1a-diff (diff c1 gc1a)
        gc1b-diff (diff c1 gc1b)
        gc2a-diff (diff c2 gc2a)
        gc2b-diff (diff c2 gc2b)]
    (assoc ind :diversifying
      (and ;(not (some #{0} [gc1a-diff gc1b-diff gc2a-diff gc2b-diff]))
           (not (some #{gc1a-diff} [gc2a-diff gc2b-diff]))
           (not (some #{gc1b-diff} [gc2a-diff gc2b-diff]))
           (not (some #{gc2a-diff} [gc1a-diff gc1b-diff]))
           (not (some #{gc2b-diff} [gc1a-diff gc1b-diff]))))))

(defn minimal-two-x-two-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        make-child #(produce-child-genome-by-autoconstruction % g g argmap)
        diff #(expressed-difference %1 %2 argmap)
        c1 (make-child g)
        c2 (make-child g)
        gc1a (make-child c1)
        gc1b (make-child c1)
        gc2a (make-child c2)
        gc2b (make-child c2)
        gc1a-diff (diff c1 gc1a)
        gc1b-diff (diff c1 gc1b)
        gc2a-diff (diff c2 gc2a)
        gc2b-diff (diff c2 gc2b)
        gc-diffs [gc1a-diff gc1b-diff gc2a-diff gc2b-diff]]
    (assoc ind :diversifying
      (and (not (some zero? gc-diffs))
           (> (count (distinct gc-diffs)) 1)))))

(defn two-x-three-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        make-child #(produce-child-genome-by-autoconstruction % g g argmap)
        diff #(expressed-difference %1 %2 argmap)
        c1 (make-child g)
        c2 (make-child g)
        gc1a (make-child c1)
        gc1b (make-child c1)
        gc1c (make-child c1)
        gc2a (make-child c2)
        gc2b (make-child c2)
        gc2c (make-child c2)
        gc1a-diff (diff c1 gc1a)
        gc1b-diff (diff c1 gc1b)
        gc1c-diff (diff c1 gc1c)
        gc2a-diff (diff c2 gc2a)
        gc2b-diff (diff c2 gc2b)
        gc2c-diff (diff c2 gc2c)]
    (assoc ind :diversifying
      (and ;(not (some zero? [gc1a-diff gc1b-diff gc1c-diff gc2a-diff gc2b-diff gc2c-diff]))
           (not (some #{gc1a-diff} [gc2a-diff gc2b-diff gc2c-diff]))
           (not (some #{gc1b-diff} [gc2a-diff gc2b-diff gc2c-diff]))
           (not (some #{gc1c-diff} [gc2a-diff gc2b-diff gc2c-diff]))
           (not (some #{gc2a-diff} [gc1a-diff gc1b-diff gc1c-diff]))
           (not (some #{gc2b-diff} [gc1a-diff gc1b-diff gc1c-diff]))
           (not (some #{gc2c-diff} [gc1a-diff gc1b-diff gc1c-diff]))))))

(defn three-gens-some-diff-diffs-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        make-child #(produce-child-genome-by-autoconstruction % % argmap)
        diff #(expressed-difference %1 %2 argmap)
        c1 (make-child g)
        c2 (make-child g)
        gc1 (make-child c1)
        gc2 (make-child c2)
        c1-diff (diff g c1)
        c2-diff (diff g c2)
        gc1-diff (diff c1 gc1)
        gc2-diff (diff c2 gc2)
        diffs [c1-diff c2-diff gc1-diff gc2-diff]]
    (assoc ind :diversifying
      (and (> (reduce min diffs) 0)
           (not= c1-diff c2-diff)
           (not= c1-diff gc1-diff)
           (not= c2-diff gc2-diff)))))

(defn size-and-instruction-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        numkids (:autoconstructive-si-children argmap)
        kids (repeatedly numkids 
                         #(produce-child-genome-by-autoconstruction g g argmap))
        kid-counts (map count kids)
        instruction-set (fn [genome]
                          (hash-set (keys (frequencies (map :instruction genome)))))
        kid-sets (map instruction-set kids)]
    (assoc ind :diversifying
      (and (not (some #{(count g)} kid-counts))
           (not (some #{(instruction-set g)} kid-sets))
           (or (< numkids 2) (not (apply = kid-counts)))
           (or (< numkids 2) (not (apply = kid-sets)))))))

(defn distinct-size-and-instruction-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        numkids (:autoconstructive-si-children argmap)
        kids (repeatedly numkids 
                         #(produce-child-genome-by-autoconstruction g g argmap))
        kid-counts (map count kids)
        instruction-set (fn [genome]
                          (hash-set (keys (frequencies (map :instruction genome)))))
        kid-sets (map instruction-set kids)]
    (assoc ind :diversifying
      (and (apply distinct? (conj kid-counts (count g)))
           (apply distinct? (conj kid-sets (instruction-set g)))))))

(defn distinct-size-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        numkids (:autoconstructive-si-children argmap)
        kids (repeatedly numkids 
                         #(produce-child-genome-by-autoconstruction g g argmap))
        kid-counts (map count kids)]
    (assoc ind :diversifying
      (apply distinct? (conj kid-counts (count g))))))

(defn three-gens-size-and-instruction-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        numkids (:autoconstructive-si-children argmap)
        make-child #(produce-child-genome-by-autoconstruction % % argmap)
        instruction-set (fn [genome]
                          (hash-set (keys (frequencies (map :instruction genome)))))
        kids (repeatedly numkids #(make-child g))
        kid-counts (map count kids)
        kid-sets (map instruction-set kids)
        grandkids (map make-child kids)
        grandkid-counts (map count grandkids)
        grandkid-sets (map instruction-set grandkids)]
    (assoc ind :diversifying
      (and (not (some #{(count g)} kid-counts))
           (not (some #{(instruction-set g)} kid-sets))
           (not (some #{(count g)} grandkid-counts))
           (not (some #{(instruction-set g)} grandkid-sets))
           (or (< numkids 2) (not (apply = (map count kids))))
           (or (< numkids 2) (not (apply = (map count grandkids))))
           (or (< numkids 2) (not (apply = (map instruction-set kids))))
           (or (< numkids 2) (not (apply = (map instruction-set grandkids))))))))

(defn safe-t-test ;; safely answers 1.0 if t-test would divide by zero
  [xvec yvec]
  (if (or (apply = xvec)
          (apply = yvec))
    1.0
    (let [new-t (TTest.)]
      (.tTest new-t (double-array xvec) (double-array yvec)))))

(defn diffmeans-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        num-children (:autoconstructive-diffmeans-children argmap)
        make-child #(produce-child-genome-by-autoconstruction % % argmap)
        c1 (make-child g)
        diffs1 (vec (repeatedly 
                      num-children 
                      #(expressed-difference c1 (make-child c1) argmap)))]
    (assoc ind :diversifying
      (if (or (some #{0} diffs1)
              (apply = diffs1))
        false
        (let [c2 (make-child g)
              diffs2 (vec (repeatedly 
                            num-children 
                            #(expressed-difference c2 (make-child c2) argmap)))]
          (if (or (some #{0} diffs2)
                  (apply = diffs2)
                  (> (safe-t-test diffs1 diffs2)
                     0.01))
            false
            true))))))

(defn minimal-reproductive-difference-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction child1 g g argmap)
        c1-diff (sequence-similarity g child1)
        c2-diff (sequence-similarity g child2)]
    (assoc ind :diversifying
      (and (not (zero? c1-diff))
           (not (zero? c2-diff))
           (not= c1-diff c2-diff)))))

(defn four-generation-reproductive-difference-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction child1 g g argmap)
        child3 (produce-child-genome-by-autoconstruction child2 g g argmap)
        c1-diff (sequence-similarity g child1)
        c2-diff (sequence-similarity g child2)
        c3-diff (sequence-similarity g child3)]
    (assoc ind :diversifying
      (and (not (zero? c1-diff))
           (not (zero? c2-diff))
           (not (zero? c3-diff))
           (distinct? c1-diff c2-diff c3-diff)))))

(defn makes-children-differently-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction g g argmap)]
    (assoc ind :diversifying
      (not= (:made-by (meta child1))
            (:made-by (meta child2))))))

(defn symbolic-makes-children-differently-diversifying?
  [ind argmap]
  (let [symbolic-made-by #(filter (comp not number?) 
                                  (flatten (:made-by (meta %))))
        g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction g g argmap)]
    (assoc ind :diversifying
      (not= (symbolic-made-by child1)
            (symbolic-made-by child2)))))

(defn makes-three-children-differently-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction g g argmap)
        child3 (produce-child-genome-by-autoconstruction g g argmap)]
    (assoc ind :diversifying
      (apply distinct? (map :made-by (map meta [child1 child2 child3]))))))

(defn symbolic-makes-three-children-differently-diversifying?
  [ind argmap]
  (let [symbolic-made-by #(filter (comp not number?) 
                                  (flatten (:made-by (meta %))))
        g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction g g argmap)
        child3 (produce-child-genome-by-autoconstruction g g argmap)]
    (assoc ind :diversifying
      (apply distinct? (map symbolic-made-by [child1 child2 child3])))))

(defn children-make-children-differently-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction g g argmap)
        gc1 (produce-child-genome-by-autoconstruction child1 g g argmap)
        gc2 (produce-child-genome-by-autoconstruction child2 g g argmap)]
    (assoc ind :diversifying
      (not= (:made-by (meta gc1))
            (:made-by (meta gc2))))))

(defn symbolic-children-make-children-differently-diversifying?
  [ind argmap]
  (let [symbolic-made-by #(filter (comp not number?) 
                                  (flatten (:made-by (meta %))))
        g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction g g argmap)
        gc1 (produce-child-genome-by-autoconstruction child1 g g argmap)
        gc2 (produce-child-genome-by-autoconstruction child2 g g argmap)]
    (assoc ind :diversifying
      (not= (symbolic-made-by gc1)
            (symbolic-made-by gc2)))))

(defn three-children-make-children-differently-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction g g argmap)
        child3 (produce-child-genome-by-autoconstruction g g argmap)
        gc1 (produce-child-genome-by-autoconstruction child1 g g argmap)
        gc2 (produce-child-genome-by-autoconstruction child2 g g argmap)
        gc3 (produce-child-genome-by-autoconstruction child3 g g argmap)]
    (assoc ind :diversifying
      (apply distinct? (map :made-by (map meta [gc1 gc2 gc3]))))))

(defn symbolic-three-children-make-children-differently-diversifying?
  [ind argmap]
  (let [symbolic-made-by #(filter (comp not number?) 
                                  (flatten (:made-by (meta %))))
        g (:genome ind)
        child1 (produce-child-genome-by-autoconstruction g g argmap)
        child2 (produce-child-genome-by-autoconstruction g g argmap)
        child3 (produce-child-genome-by-autoconstruction g g argmap)
        gc1 (produce-child-genome-by-autoconstruction child1 g g argmap)
        gc2 (produce-child-genome-by-autoconstruction child2 g g argmap)
        gc3 (produce-child-genome-by-autoconstruction child3 g g argmap)]
    (assoc ind :diversifying
      (apply distinct? (map symbolic-made-by [gc1 gc2 gc3])))))

(defn reproductive-change-changes-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (flatten (:made-by (meta c)))
        gc (produce-child-genome-by-autoconstruction c g g argmap)
        gc-made-by (flatten (:made-by (meta gc)))
        ggc (produce-child-genome-by-autoconstruction gc g g argmap)
        ggc-made-by (flatten (:made-by (meta ggc)))]
    (assoc ind :diversifying
      (distinct? 1 
                 (sequence-similarity c-made-by gc-made-by)
                 (sequence-similarity gc-made-by ggc-made-by)))))

(defn symbolic-reproductive-change-changes-diversifying?
  [ind argmap]
  (let [symbolic #(filter (comp not number?) %)
        g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (symbolic (flatten (:made-by (meta c))))
        gc (produce-child-genome-by-autoconstruction c g g argmap)
        gc-made-by (symbolic (flatten (:made-by (meta gc))))
        ggc (produce-child-genome-by-autoconstruction gc g g argmap)
        ggc-made-by (symbolic (flatten (:made-by (meta ggc))))]
    (assoc ind :diversifying
      (distinct? 1 
                 (sequence-similarity c-made-by gc-made-by)
                 (sequence-similarity gc-made-by ggc-made-by)))))

(defn symbolic-reproductive-change-diversifying?
  [ind argmap]
  (let [symbolic #(filter (comp not number?) %)
        g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (symbolic (flatten (:made-by (meta c))))
        gc (produce-child-genome-by-autoconstruction c g g argmap)
        gc-made-by (symbolic (flatten (:made-by (meta gc))))]
    (assoc ind :diversifying
      (not= c-made-by gc-made-by))))

(defn reproductive-change-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (:made-by (meta c))
        gc (produce-child-genome-by-autoconstruction c g g argmap)
        gc-made-by (:made-by (meta gc))]
    (assoc ind :diversifying
      (not= c-made-by gc-made-by))))

(defn symbolic-reproductive-divergence-diversifying?
  [ind argmap]
  (let [symbolic #(filter (comp not number?) %)
        g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c2 (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (symbolic (flatten (:made-by (meta c))))
        c2-made-by (symbolic (flatten (:made-by (meta c2))))]
    (assoc ind :diversifying
      (not= c-made-by c2-made-by))))

(defn three-way-symbolic-reproductive-divergence-diversifying?
  [ind argmap]
  (let [symbolic #(filter (comp not number?) %)
        g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c2 (produce-child-genome-by-autoconstruction g g argmap)
        c3 (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (symbolic (flatten (:made-by (meta c))))
        c2-made-by (symbolic (flatten (:made-by (meta c2))))
        c3-made-by (symbolic (flatten (:made-by (meta c3))))]
    (assoc ind :diversifying
      (distinct? c-made-by c2-made-by c3-made-by))))

(defn reproductive-divergence-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c2 (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (flatten (:made-by (meta c)))
        c2-made-by (flatten (:made-by (meta c2)))]
    (assoc ind :diversifying
      (not= c-made-by c2-made-by))))

(defn three-way-reproductive-divergence-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c2 (produce-child-genome-by-autoconstruction g g argmap)
        c3 (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (flatten (:made-by (meta c)))
        c2-made-by (flatten (:made-by (meta c2)))
        c3-made-by (flatten (:made-by (meta c3)))]
    (assoc ind :diversifying
      (distinct? c-made-by c2-made-by c3-made-by))))

(defn reproductive-change-changes-differently-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c2 (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (flatten (:made-by (meta c)))
        c2-made-by (flatten (:made-by (meta c2)))
        gc (produce-child-genome-by-autoconstruction c g g argmap)
        gc2 (produce-child-genome-by-autoconstruction c2 g g argmap)
        gc-made-by (flatten (:made-by (meta gc)))
        gc2-made-by (flatten (:made-by (meta gc2)))
        ggc (produce-child-genome-by-autoconstruction gc g g argmap)
        ggc2 (produce-child-genome-by-autoconstruction gc2 g g argmap)
        ggc-made-by (flatten (:made-by (meta ggc)))
        ggc2-made-by (flatten (:made-by (meta ggc2)))
        mbsim-c-gc (sequence-similarity c-made-by gc-made-by)
        mbsim-c2-gc2 (sequence-similarity c2-made-by gc2-made-by)
        mbsim-gc-ggc (sequence-similarity gc-made-by ggc-made-by)
        mbsim-gc2-ggc2 (sequence-similarity gc2-made-by ggc2-made-by)]
    (assoc ind :diversifying
      (and (distinct? 1 mbsim-c-gc mbsim-gc-ggc)
           (distinct? 1 mbsim-c2-gc2 mbsim-gc2-ggc2)
           ;(distinct? mbsim-gc-ggc mbsim-gc2-ggc2)
           (distinct? (- mbsim-c-gc mbsim-gc-ggc)
                      (- mbsim-c2-gc2 mbsim-gc2-ggc2))))))

(defn symbolic-reproductive-change-changes-differently-diversifying?
  [ind argmap]
  (let [symbolic #(filter (comp not number?) %)
        g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c2 (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (symbolic (flatten (:made-by (meta c))))
        c2-made-by (symbolic (flatten (:made-by (meta c2))))
        gc (produce-child-genome-by-autoconstruction c g g argmap)
        gc2 (produce-child-genome-by-autoconstruction c2 g g argmap)
        gc-made-by (symbolic (flatten (:made-by (meta gc))))
        gc2-made-by (symbolic (flatten (:made-by (meta gc2))))
        ggc (produce-child-genome-by-autoconstruction gc g g argmap)
        ggc2 (produce-child-genome-by-autoconstruction gc2 g g argmap)
        ggc-made-by (symbolic (flatten (:made-by (meta ggc))))
        ggc2-made-by (symbolic (flatten (:made-by (meta ggc2))))
        mbsim-c-gc (sequence-similarity c-made-by gc-made-by)
        mbsim-c2-gc2 (sequence-similarity c2-made-by gc2-made-by)
        mbsim-gc-ggc (sequence-similarity gc-made-by ggc-made-by)
        mbsim-gc2-ggc2 (sequence-similarity gc2-made-by ggc2-made-by)]
    (assoc ind :diversifying
      (and (distinct? 1 mbsim-c-gc mbsim-gc-ggc)
           (distinct? 1 mbsim-c2-gc2 mbsim-gc2-ggc2)
           (distinct? mbsim-gc-ggc mbsim-gc2-ggc2) ;**
           (distinct? (- mbsim-c-gc mbsim-gc-ggc)
                      (- mbsim-c2-gc2 mbsim-gc2-ggc2))))))

(defn symbolic-three-way-reproductive-change-changes-differently-diversifying?
  [ind argmap]
  (let [symbolic #(filter (comp not number?) %)
        g (:genome ind)
        c (produce-child-genome-by-autoconstruction g g argmap)
        c2 (produce-child-genome-by-autoconstruction g g argmap)
        c3 (produce-child-genome-by-autoconstruction g g argmap)
        c-made-by (symbolic (flatten (:made-by (meta c))))
        c2-made-by (symbolic (flatten (:made-by (meta c2))))
        c3-made-by (symbolic (flatten (:made-by (meta c3))))
        gc (produce-child-genome-by-autoconstruction c g g argmap)
        gc2 (produce-child-genome-by-autoconstruction c2 g g argmap)
        gc3 (produce-child-genome-by-autoconstruction c3 g g argmap)
        gc-made-by (symbolic (flatten (:made-by (meta gc))))
        gc2-made-by (symbolic (flatten (:made-by (meta gc2))))
        gc3-made-by (symbolic (flatten (:made-by (meta gc3))))
        ggc (produce-child-genome-by-autoconstruction gc g g argmap)
        ggc2 (produce-child-genome-by-autoconstruction gc2 g g argmap)
        ggc3 (produce-child-genome-by-autoconstruction gc3 g g argmap)
        ggc-made-by (symbolic (flatten (:made-by (meta ggc))))
        ggc2-made-by (symbolic (flatten (:made-by (meta ggc2))))
        ggc3-made-by (symbolic (flatten (:made-by (meta ggc3))))
        mbsim-c-gc (sequence-similarity c-made-by gc-made-by)
        mbsim-c2-gc2 (sequence-similarity c2-made-by gc2-made-by)
        mbsim-c3-gc3 (sequence-similarity c3-made-by gc3-made-by)
        mbsim-gc-ggc (sequence-similarity gc-made-by ggc-made-by)
        mbsim-gc2-ggc2 (sequence-similarity gc2-made-by ggc2-made-by)
        mbsim-gc3-ggc3 (sequence-similarity gc3-made-by ggc3-made-by)]
    (assoc ind :diversifying
      (and (distinct? 1 mbsim-c-gc mbsim-gc-ggc)
           (distinct? 1 mbsim-c2-gc2 mbsim-gc2-ggc2)
           (distinct? 1 mbsim-c3-gc3 mbsim-gc3-ggc3)
           (distinct? (- mbsim-c-gc mbsim-gc-ggc)
                      (- mbsim-c2-gc2 mbsim-gc2-ggc2)
                      (- mbsim-c3-gc3 mbsim-gc3-ggc3))))))

(defn use-mate-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        mate (vec (repeat (count g) {:instruction :from-mate :close 0}))
        c (produce-child-genome-by-autoconstruction g mate argmap)]
    (assoc ind :diversifying
      (some #(= (:instruction %) :from-mate) c))))

(defn use-mate-differently-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        mate (vec (repeat (count g) {:instruction :from-mate :close 0}))
        c1 (produce-child-genome-by-autoconstruction g mate argmap)
        c1-from-mate-count (count (filter #(= (:instruction %) :from-mate) c1))]
    (assoc ind :diversifying
      (if (> c1-from-mate-count 0)
        (let [c2 (produce-child-genome-by-autoconstruction g mate argmap)
              c2-from-mate-count (count (filter #(= (:instruction %) :from-mate) c2))]
          (and (> c2-from-mate-count 0)
               (not= c1-from-mate-count c2-from-mate-count)))
        false))))

(defn si-and-mate-use-diversifying?
  [ind argmap]
  (let [g (:genome ind)
        mate (vec (repeat (count g) {:instruction :from-mate :close 0}))
        c1 (produce-child-genome-by-autoconstruction g mate argmap)
        c1-from-mate-count (count (filter #(= (:instruction %) :from-mate) c1))]
    (assoc ind :diversifying
      (if (> c1-from-mate-count 0)
        (let [c1-not-from-mate (filter #(not= (:instruction %) :from-mate) c1)
              c1-not-from-mate-count (count c1-not-from-mate)
              g-count (count g)
              c1-instructions (set (distinct (map :instruction c1-not-from-mate)))
              g-instructions (set (distinct (map :instruction g)))]
          (if (and (not= c1-instructions g-instructions)
                   (not= c1-not-from-mate-count g-count))
            (let [c2 (produce-child-genome-by-autoconstruction g mate argmap)
                  c2-from-mate-count (count (filter #(= (:instruction %) :from-mate) c2))]
              (if (and (> c2-from-mate-count 0)
                       (not= c1-from-mate-count c2-from-mate-count))
                (let [c2-not-from-mate (filter #(not= (:instruction %) :from-mate) c2)
                      c2-not-from-mate-count (count c2-not-from-mate)
                      c2-instructions (set (distinct (map :instruction c2-not-from-mate)))]
                  (and (not= c2-instructions g-instructions)
                       (not= c2-instructions c1-instructions)
                       (not= c2-not-from-mate-count g-count)
                       (not= c2-not-from-mate-count c1-not-from-mate-count)))
                false))
            false))
        false))))

(defn different-errors-diversifying?
  [ind argmap]
  (if (and (:parent1-errors argmap)
           (:parent2-errors argmap))
    (let [errs (or (:errors ind)
                   (do
                     (swap! evaluations-count inc)
                     (:errors ((:error-function argmap)
                               {:genome (:genome ind)
                                :program (translate-plush-genome-to-push-program
                                           {:genome (:genome ind)}
                                           argmap)}))))]
      (assoc ind :diversifying
        (and (not= errs (take (count errs) (:parent1-errors argmap)))
             (not= errs (take (count errs) (:parent2-errors argmap))))))
    (assoc ind :diversifying true)))

(defn new-errors-prospective-when-necessary-diversifying?
  [ind argmap]
  (if (not (:print-history argmap))
    (throw
     (Exception.
      ":print-history must be true for :new-errors-prospective-when-necessary diversification test"))
    (let [errs (or (:errors ind)
                   (do
                     (swap! evaluations-count inc)
                     (:errors ((:error-function argmap)
                               {:genome (:genome ind)
                                :program (translate-plush-genome-to-push-program
                                          {:genome (:genome ind)}
                                          argmap)}))))]
      (assoc ind :diversifying
             (if (not (empty? (:parent1-history argmap)))
               (not (some #{errs} (:parent1-history argmap)))
               (let [g (:genome ind)
                     c (produce-child-genome-by-autoconstruction g g argmap)]
                 (if (empty? c)
                   false
                   (not= errs
                         (do
                           (swap! evaluations-count inc)
                           ((:error-function argmap)
                            {:genome c
                             :program (translate-plush-genome-to-push-program
                                       {:genome c}
                                       argmap)}))))))))))

(defn new-instruction-diversifying?
  [ind {:keys [parent1-genome parent2-genome] :as argmap}]
  (let [child-instructions (set (map :instruction (:genome ind)))
        parent-instructions (set (map :instruction
                                      (concat parent1-genome parent2-genome)))]
    (assoc ind :diversifying 
      (not (empty? (clojure.set/difference child-instructions parent-instructions))))))

(defn lost-instruction-diversifying?
  [ind {:keys [parent1-genome parent2-genome] :as argmap}]
  (let [set-diff clojure.set/difference
        child-instructions (set (map :instruction (:genome ind)))
        parent1-instructions (set (map :instruction parent1-genome))
        parent2-instructions (set (map :instruction parent2-genome))]
    (assoc ind :diversifying 
      (and (not (empty? (set-diff parent1-instructions child-instructions)))
           (not (empty? (set-diff parent2-instructions child-instructions)))))))

(defn different-instructions-diversifying?
  [ind {:keys [parent1-genome parent2-genome] :as argmap}]
  (assoc ind :diversifying 
    (and (not= (set (map :instruction (:genome ind)))
               (set (map :instruction parent1-genome)))
         (not= (set (map :instruction (:genome ind)))
               (set (map :instruction parent2-genome))))))

(defn new-size-diversifying?
  [ind {:keys [parent1-genome parent2-genome] :as argmap}]
  (let [child-size (count (:genome ind))]
    (assoc ind :diversifying
      (and (not= child-size (count parent1-genome))
           (not= child-size (count parent2-genome))))))

(defn checks-autoconstructing-diversifying?
  [ind argmap]
  (assoc ind :diversifying
    (some (fn [instruction-map]
            (and (= (:instruction instruction-map) 'genome_autoconstructing)
                 (not (:silent instruction-map))))
          (:genome ind))))

(defn autoconstruction-aware-diversifying?
  [ind argmap]
  (assoc ind :diversifying
    (some (fn [instruction-map]
            (and (not (:silent instruction-map))
                 (or (= (:instruction instruction-map) 'genome_autoconstructing)
                     (= (:instruction instruction-map) 'genome_if_autoconstructing))))
          (:genome ind))))

(defn contains-genesis-diversifying?
  [ind argmap]
  (assoc ind :diversifying
    (some (fn [instruction-map]
            (and (not (:silent instruction-map))
                 (= (:instruction instruction-map) 'genome_genesis)))
          (:genome ind))))

(defn diversifying?
  "Returns ind with :diversifying set to true if it staisfies all test
  specified in (:autoconstructive-diversification-test argmap), or false
  otherwise."
  [ind argmap]
  (loop [i (assoc ind :diversifying true)
         tests (let [raw-tests (:autoconstructive-diversification-test argmap)]
                 (if (coll? raw-tests) raw-tests [raw-tests]))]
    (if (or (empty? tests)
            (not (:diversifying i)))
      i
      (recur ((case (first tests)
                :gecco2016 gecco2016-diversifying?
                :gecco2016-plus1 gecco2016-plus1-diversifying?
                :gecco2016-plus2 gecco2016-plus2-diversifying?
                :three-gens-diff-diffs three-gens-diff-diffs-diversifying?
                :three-gens-same-inputs-diff-diffs three-gens-same-inputs-diff-diffs-diversifying?
                :four-gens-same-inputs-diff-diffs four-gens-same-inputs-diff-diffs-diversifying?
                :two-x-two two-x-two-diversifying?
                :minimal-two-x-two minimal-two-x-two-diversifying?
                :two-x-three two-x-three-diversifying?
                :three-gens-some-diff-diffs three-gens-some-diff-diffs-diversifying?
                :size-and-instruction size-and-instruction-diversifying?
                :distinct-size-and-instruction distinct-size-and-instruction-diversifying?
                :distinct-size distinct-size-diversifying?
                :three-gens-size-and-instruction three-gens-size-and-instruction-diversifying?
                :diffmeans diffmeans-diversifying?
                :minimal-reproductive-difference minimal-reproductive-difference-diversifying?
                :four-generation-reproductive-difference four-generation-reproductive-difference-diversifying?
                :makes-children-differently makes-children-differently-diversifying?
                :symbolic-makes-children-differently symbolic-makes-children-differently-diversifying?
                :makes-three-children-differently makes-three-children-differently-diversifying?
                :symbolic-makes-three-children-differently symbolic-makes-three-children-differently-diversifying?
                :children-make-children-differently children-make-children-differently-diversifying?
                :symbolic-children-make-children-differently symbolic-children-make-children-differently-diversifying?
                :three-children-make-children-differently three-children-make-children-differently-diversifying?
                :symbolic-three-children-make-children-differently symbolic-three-children-make-children-differently-diversifying?
                :symbolic-reproductive-change-changes symbolic-reproductive-change-changes-diversifying?
                :symbolic-reproductive-change-changes-differently symbolic-reproductive-change-changes-differently-diversifying?
                :symbolic-three-way-reproductive-change-changes-differently symbolic-three-way-reproductive-change-changes-differently-diversifying?
                :symbolic-reproductive-change symbolic-reproductive-change-diversifying?
                :reproductive-change reproductive-change-diversifying?
                :symbolic-reproductive-divergence symbolic-reproductive-divergence-diversifying?
                :reproductive-divergence reproductive-divergence-diversifying?
                :three-way-symbolic-reproductive-divergence symbolic-reproductive-divergence-diversifying?
                :three-way-reproductive-divergence reproductive-divergence-diversifying?
                :reproductive-change-changes reproductive-change-changes-diversifying?
                :reproductive-change-changes-differently reproductive-change-changes-differently-diversifying?
                :use-mate use-mate-diversifying?
                :use-mate-differently use-mate-differently-diversifying?
                :si-and-mate-use si-and-mate-use-diversifying?
                :doesnt-clone doesnt-clone-diversifying?
                :doesnt-clone-genetically doesnt-clone-genetically-diversifying?
                :child-doesnt-clone child-doesnt-clone-diversifying?
                :not-a-clone not-a-clone-diversifying?
                :lineage-behavior lineage-behavior-diversifying?
                :not-empty not-empty-diversifying?
                :minimum-genetic-difference minimum-genetic-difference-diversifying?
                :different-errors different-errors-diversifying?
                :new-errors new-errors-diversifying?
                :new-errors-prospective-when-necessary new-errors-prospective-when-necessary-diversifying?
                :enough-new-errors enough-new-errors-diversifying?
                :at-least-half-new-errors at-least-half-new-errors-diversifying?
                :new-instruction new-instruction-diversifying?
                :lost-instruction lost-instruction-diversifying?
                :different-instructions different-instructions-diversifying?
                :new-size new-size-diversifying?
                :checks-autoconstructing checks-autoconstructing-diversifying?
                :autoconstruction-aware autoconstruction-aware-diversifying?
                :contains-genesis contains-genesis-diversifying?
                :none (fn [ind argmap] i))
              i
              argmap)
             (rest tests)))))

(defn autoconstruction
  "Returns a genome for a child produced either by autoconstruction (executing parent1
  with both parents on top of the genome stack and also available via input instructions)
  or by cloning. In either case if the child is not diversifying then a random
  genome is returned instead IF that is itself diversifying; if it isn't then an empty 
  genome is returned. The construct/clone ration is hardcoded here, but might
  be set globally or eliminated in the future."
  [parent1 parent2 {:keys [maintain-ancestors atom-generators max-genome-size-in-initial-program 
                           autoconstructive-clone-probability autoconstructive-decay
                           autoconstructive-parent-decay autoconstructive-clone-decay]
                    :as argmap}]
  (let [decay (fn [g rate-or-rates]
                (let [rate (random-element-or-identity-if-not-a-collection rate-or-rates)]
                  (if (zero? rate)
                    g
                    (vec (filter identity (map #(if (< (lrand) rate) nil %) g))))))
        parent1-genome (decay (:genome parent1) autoconstructive-parent-decay)
        parent2-genome (decay (:genome parent2) autoconstructive-parent-decay)
        clone (<= (lrand) autoconstructive-clone-probability)
        pre-decay-child-genome (if clone
                                 parent1-genome
                                 (produce-child-genome-by-autoconstruction
                                  parent1-genome parent2-genome argmap))
        child-genome (if (and clone (not= autoconstructive-clone-decay :same))
                       (decay pre-decay-child-genome autoconstructive-clone-decay)
                       (decay pre-decay-child-genome autoconstructive-decay))
        checked (diversifying? {:genome child-genome}
                               (-> argmap
                                   (assoc :parent1-genome parent1-genome)
                                   (assoc :parent2-genome parent2-genome)
                                   (assoc :parent1-errors (:errors parent1))
                                   (assoc :parent2-errors (:errors parent2))
                                   (assoc :parent1-history (:history parent1))
                                   (assoc :parent2-history (:history parent2))))]
    (if (:diversifying checked)
      (assoc (make-individual :genome child-genome
                              :errors (:errors checked)
                              :history (:history parent1)
                              :age ((age-combining-function argmap) parent1 parent2 child-genome)
                              :grain-size (compute-grain-size child-genome parent1 parent2 argmap)
                              :ancestors (if maintain-ancestors
                                           (cons (:genome parent1) (:ancestors parent1))
                                           (:ancestors parent1))
                              :is-random-replacement false)
             :parent1-genome parent1-genome
             :parent2-genome parent2-genome
             :parent1-errors (:errors parent1)
             :parent2-errors (:errors parent2)
             :diversifying true)
      (let [new-genome (random-plush-genome
                        max-genome-size-in-initial-program atom-generators argmap)
            new-checked (diversifying? {:genome new-genome} argmap)]
        (if (:diversifying new-checked)
          (assoc (make-individual :genome new-genome
                                  :errors (:errors new-checked)
                                  :history ()
                                  :age 0
                                  :grain-size (compute-grain-size new-genome argmap)
                                  :ancestors ()
                                  :is-random-replacement true)
                 :diversifying true)
          (make-individual :genome []
                           :errors nil
                           :history ()
                           :age 0
                           :grain-size (compute-grain-size [] argmap)
                           :ancestors ()
                           :is-random-replacement true))))))
