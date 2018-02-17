(ns clojush.pushgp.genetic-operators
  (:use [clojush util random individual globals interpreter translate pushstate]
        clojush.instructions.tag
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
  "Returns parent"
  [ind argmap]
  ind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genesis

(defn genesis
  "Ignores the provided parent and returns a new, random individual, with age 0."
  [ind {:keys [maintain-ancestors max-genome-size-in-initial-program atom-generators]
        :as argmap}]
  (let [genome (random-plush-genome max-genome-size-in-initial-program 
                                    atom-generators 
                                    argmap)]
    (make-individual :genome genome
                     :history (:history ind)
                     :age 0
                     :grain-size (compute-grain-size genome argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

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
  [instr-map uniform-mutation-tag-gaussian-standard-deviation]
  (let [instr (:instruction instr-map)
        tagparts (string/split (name instr) #"_")
        tag-num (read-string (last tagparts))
        new-tag-num (mod (round (perturb-with-gaussian-noise 
                                  uniform-mutation-tag-gaussian-standard-deviation tag-num))
                         @global-tag-limit)
        new-instr (symbol (apply str (interpose "_" (concat (butlast tagparts) 
                                                            (list (str new-tag-num))))))]
    (assoc instr-map :instruction new-instr)))

(defn uniform-mutation
  "Uniformly mutates individual. For each token in the genome, there is
   uniform-mutation-rate probability of being mutated. If a token is to be
   mutated, it has a uniform-mutation-constant-tweak-rate probability of being
   mutated using a constant mutator (which varies depending on the type of the
   token), and otherwise is replaced with a random instruction."
  [ind {:keys [uniform-mutation-rate uniform-mutation-constant-tweak-rate
               uniform-mutation-float-gaussian-standard-deviation
               uniform-mutation-int-gaussian-standard-deviation
               uniform-mutation-tag-gaussian-standard-deviation
               uniform-mutation-string-char-change-rate maintain-ancestors
               atom-generators]
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
                              (assoc token
                                     :instruction
                                     (:instruction 
                                       (first (random-plush-genome 1 atom-generators argmap)))))
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (tag-instruction? const)
                               (tag-gaussian-tweak token 
                                                   uniform-mutation-tag-gaussian-standard-deviation)
                               (assoc token
                                      :instruction
                                      (cond
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
                                        ;; anything else
                                        (lrand-nth [true false])
                                        :else 
                                        (:instruction 
                                          (first (random-plush-genome 1 atom-generators argmap))))))))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-rate)
                          (if (< (lrand) uniform-mutation-constant-tweak-rate)
                            (constant-mutator token)
                            (instruction-mutator token))
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
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
                     :age (inc (:age ind))
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
                     :age (inc (:age ind))
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
                     :age (inc (:age ind))
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform tag mutation

(defn uniform-tag-mutation
  "Uniformly mutates individual. For each tag instruction in the genome, there is
   uniform-mutation-rate probability of being mutated."
  [ind {:keys [uniform-mutation-rate uniform-mutation-tag-gaussian-standard-deviation
               maintain-ancestors atom-generators]
        :as argmap}]
  (let [uniform-mutation-rate 
        (random-element-or-identity-if-not-a-collection uniform-mutation-rate)
        
        uniform-mutation-tag-gaussian-standard-deviation
        (random-element-or-identity-if-not-a-collection uniform-mutation-tag-gaussian-standard-deviation)
         
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (tag-instruction? const)
                               (tag-gaussian-tweak token 
                                                   uniform-mutation-tag-gaussian-standard-deviation)
                               token)))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-rate)
                          (constant-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
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
                     :age (inc (:age ind))
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
                     :age (inc (:age ind))
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
                       :age (inc (:age ind))
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
                       :age (inc (:age ind))
                       :grain-size (compute-grain-size new-genome ind argmap)
                       :ancestors (if maintain-ancestors
                                    (cons (:genome ind) (:ancestors ind))
                                    (:ancestors ind))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform deletion

(defn uniform-deletion
  "Returns the individual with each element of its genome possibly deleted, with probability
given by uniform-deletion-rate."
  [ind {:keys [uniform-deletion-rate maintain-ancestors] :as argmap}]
  (let [rate (random-element-or-identity-if-not-a-collection uniform-deletion-rate)
        new-genome (vec (filter identity
                                (mapv #(if (< (lrand) rate) nil %)
                                      (:genome ind))))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform addition

(defn uniform-addition
  "Returns the individual with each element of its genome possibly preceded or followed by
  a new gene, with probability given by uniform-addition-rate."
  [ind {:keys [uniform-addition-rate maintain-ancestors atom-generators] :as argmap}]
  (let [rate (random-element-or-identity-if-not-a-collection uniform-addition-rate)
        new-genome (vec (apply concat
                               (mapv #(if (< (lrand) rate)
                                        (lshuffle [% 
                                                   (random-plush-instruction-map
                                                     atom-generators argmap)])
                                        [%])
                                     (:genome ind))))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
                     :grain-size (compute-grain-size new-genome ind argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform addition and deletion

(defn uniform-addition-and-deletion
  "Returns the individual after two passes of mutation. In the first pass, each element of 
  its genome may possibly be preceded or followed by a new gene. In the second pass, each
  element of the genome may possibly be deleted. Probabilities are given by 
  uniform-addition-and-deletion-rate."
  [ind {:keys [uniform-addition-and-deletion-rate maintain-ancestors atom-generators] 
        :as argmap}]
  (let [addition-rate (random-element-or-identity-if-not-a-collection uniform-addition-and-deletion-rate)
        deletion-rate (if (zero? addition-rate)
                        0
                        (/ 1 (+ (/ 1 addition-rate) 1)))
        after-addition (vec (apply concat
                                   (mapv #(if (< (lrand) addition-rate)
                                            (lshuffle [% 
                                                       (random-plush-instruction-map
                                                         atom-generators argmap)])
                                            [%])
                                         (:genome ind))))
        new-genome (vec (filter identity
                                (mapv #(if (< (lrand) deletion-rate) nil %)
                                      after-addition)))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
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
  Probability is given by uniform-combination-rate."
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
                     :age ((age-combining-function argmap) parent1 parent2 new-genome)
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
  uniform-combination-and-deletion-rate."
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
                     :age ((age-combining-function argmap) parent1 parent2 new-genome)
                     :grain-size (compute-grain-size new-genome parent1 parent2 argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome parent1) (:ancestors parent1))
                                  (:ancestors parent1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alternation

(defn alternation
  "Uniformly alternates between the two parents using a similar method to that
   used in ULTRA."
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
                     :age ((age-combining-function argmap) parent1 parent2 new-genome)
                     :grain-size (compute-grain-size new-genome parent1 parent2 argmap)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome parent1) (:ancestors parent1))
                                  (:ancestors parent1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; two-point crossover
(defn two-point-crossover
  "Crossover of two parents. Each parent will have two points randomly selected, and
   the code between the two points in the first parent will be replaced by the
   code between the two points in the second parent."
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
                     :age ((age-combining-function argmap) parent1 parent2 new-genome)
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
   will be taken from one of the two parents at random."
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
                       :age ((age-combining-function argmap) parent1 parent2 new-genome)
                       :grain-size (compute-grain-size new-genome parent1 parent2 argmap)
                       :ancestors (if maintain-ancestors
                                    (cons (:genome parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoconstuction

;; NOTE: EXPERIMENTAL!

(defn process-genome-for-autoconstruction
  "Replaces input instructions with noops and  autoconstructive_<type>_rand
  with <type>_rand."
  [genome]
  (let [input-instruction? 
        (fn [instruction]
          (and (symbol? instruction)
               (or (re-seq #"in\d+" (name instruction)) ;; from input-output
                   (re-seq #"in_dm" (name instruction)) ;; from digital-multiplier
                   (some #{instruction}
                         '(a0 a1 a2 d0 d1 d2 d3 d4 d5 d6 d7)))))] ;; from mux problems
    (mapv (fn [instruction-map]
            (cond 
              (input-instruction? (:instruction instruction-map))
              (assoc instruction-map :instruction 'code_noop)
              ;
              (= (:instruction instruction-map) 'autoconstructive_integer_rand)
              (assoc instruction-map :instruction 'integer_rand)
              ;
              (= (:instruction instruction-map) 'autoconstructive_boolean_rand)
              (assoc instruction-map :instruction 'boolean_rand)
              ;
              :else
              instruction-map))
         genome)))

(defn produce-child-genome-by-autoconstruction
  "Runs the program expressed by parent1-genome with both parent genomes
on the genome stack and also available via input instructions, and returns
the resulting top genome."
  ([parent1-genome parent2-genome argmap]
   (produce-child-genome-by-autoconstruction 
     parent1-genome parent1-genome parent2-genome argmap))
  ([genome-to-run parent1-genome parent2-genome argmap]
   (let [run-result (top-item :genome
                              (run-push
                                (translate-plush-genome-to-push-program
                                  {:genome
                                   (process-genome-for-autoconstruction genome-to-run)}
                                  argmap)
                                (-> (->> (make-push-state)
                                         (push-item parent2-genome :genome)
                                         (push-item parent1-genome :genome))
                                    (assoc :parent1-genome parent1-genome)
                                    (assoc :parent2-genome parent2-genome)
                                    (assoc :autoconstructing true))))]
     (if (or (seq? run-result) (vector? run-result))
       (vec run-result)
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

(defn new-instruction-diversifying?
  [ind {:keys [parent1-genome parent2-genome] :as argmap}]
  (let [child-instructions (set (map :instruction (:genome ind)))
        parent-instructions (set (map :instruction
                                      (concat parent1-genome parent2-genome)))]
    (assoc ind :diversifying 
      (not (empty? (clojure.set/difference child-instructions parent-instructions))))))

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
                :three-gens-diff-diffs three-gens-diff-diffs-diversifying?
                :three-gens-same-inputs-diff-diffs three-gens-same-inputs-diff-diffs-diversifying?
                :four-gens-same-inputs-diff-diffs four-gens-same-inputs-diff-diffs-diversifying?
                :three-gens-some-diff-diffs three-gens-some-diff-diffs-diversifying?
                :size-and-instruction size-and-instruction-diversifying?
                :distinct-size-and-instruction distinct-size-and-instruction-diversifying?
                :distinct-size distinct-size-diversifying?
                :three-gens-size-and-instruction three-gens-size-and-instruction-diversifying?
                :diffmeans diffmeans-diversifying?
                :minimal-reproductive-difference minimal-reproductive-difference-diversifying?
                :use-mate use-mate-diversifying?
                :use-mate-differently use-mate-differently-diversifying?
                :si-and-mate-use si-and-mate-use-diversifying?
                :doesnt-clone doesnt-clone-diversifying?
                :doesnt-clone-genetically doesnt-clone-genetically-diversifying?
                :child-doesnt-clone child-doesnt-clone-diversifying?
                :not-a-clone not-a-clone-diversifying?
                :minimum-genetic-difference minimum-genetic-difference-diversifying?
                :different-errors different-errors-diversifying?
                :new-instruction-diversifying new-instruction-diversifying?
                :new-size-diversifying new-size-diversifying?
                :checks-autoconstructing checks-autoconstructing-diversifying?
                :autoconstruction-aware autoconstruction-aware-diversifying?
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
                           autoconstructive-parent-decay]
                    :as argmap}]
  (let [decay (fn [g rate]
                (if (zero? rate)
                  g
                  (vec (filter identity (map #(if (< (lrand) rate) nil %) g)))))
        parent1-genome (decay (:genome parent1) autoconstructive-parent-decay)
        parent2-genome (decay (:genome parent2) autoconstructive-parent-decay)
        clone (<= (lrand) autoconstructive-clone-probability)
        pre-decay-child-genome (if clone
                                   parent1-genome
                                   (produce-child-genome-by-autoconstruction 
                                     parent1-genome parent2-genome argmap))
        child-genome (decay pre-decay-child-genome autoconstructive-decay)
        checked (diversifying? {:genome child-genome}
                               (-> argmap
                                   (assoc :parent1-genome parent1-genome)
                                   (assoc :parent2-genome parent2-genome)
                                   (assoc :parent1-errors (:errors parent1))
                                   (assoc :parent2-errors (:errors parent2))))]
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
        :parent2-genome parent2-genome)
      (let [new-genome (random-plush-genome 
                         max-genome-size-in-initial-program atom-generators argmap)
            new-checked (diversifying? {:genome new-genome} argmap)]
        (if (:diversifying new-checked)
          (make-individual :genome new-genome
                           :errors (:errors new-checked)
                           :history ()
                           :age 0
                           :grain-size (compute-grain-size new-genome argmap)
                           :ancestors ()
                           :is-random-replacement true)
          (make-individual :genome []
                           :errors nil
                           :history ()
                           :age 0
                           :grain-size (compute-grain-size [] argmap)
                           :ancestors ()
                           :is-random-replacement true))))))

