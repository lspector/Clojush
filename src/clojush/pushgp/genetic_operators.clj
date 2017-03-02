(ns clojush.pushgp.genetic-operators
  (:use [clojush util random individual globals interpreter translate pushstate]
        clojush.instructions.tag
        [clojure.math.numeric-tower])
  (:import (org.apache.commons.math3.stat.inference TTest))
  (:require [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local utilities

(defn number
  "If given a number, returns it. If given a collection, returns a member of the collection.
  Intended for allowing arguments to genetic operators, such as mutation rates, to take
  collections in addition to single values"
  [number-or-numbers]
  (if (number? number-or-numbers)
    number-or-numbers
    (lrand-nth number-or-numbers)))

(defn age-combining-function
  "Returns the actual age combining function specified by the :age-combining-function
  in the argmap."
  [argmap]
  (case (:age-combining-function argmap)
    :max max
    :min min
    :average average))

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
  (make-individual :genome (random-plush-genome max-genome-size-in-initial-program 
                                                atom-generators 
                                                argmap)
                   :history (:history ind)
                   :age 0
                   :ancestors (if maintain-ancestors
                                (cons (:genome ind) (:ancestors ind))
                                (:ancestors ind))))

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
        (number uniform-mutation-rate)
        
        uniform-mutation-constant-tweak-rate 
        (number uniform-mutation-constant-tweak-rate)
        
        uniform-mutation-float-gaussian-standard-deviation 
        (number uniform-mutation-float-gaussian-standard-deviation)
        
        uniform-mutation-int-gaussian-standard-deviation 
        (number uniform-mutation-int-gaussian-standard-deviation)
        
        uniform-mutation-tag-gaussian-standard-deviation 
        (number uniform-mutation-tag-gaussian-standard-deviation)
        
        uniform-mutation-string-char-change-rate 
        (number uniform-mutation-string-char-change-rate)
    
        string-tweak (fn [st]
                       (apply str (map (fn [c]
                                         (if (< (lrand) uniform-mutation-string-char-change-rate)
                                           (lrand-nth (concat ["\n" "\t"] 
                                                              (map (comp str char) (range 32 127))))
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
  (let [uniform-mutation-rate (number uniform-mutation-rate)
        instruction-mutator (fn [token]
                              (assoc token
                                :instruction
                                (:instruction (first (random-plush-genome 1 atom-generators argmap)))))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-rate)
                          (instruction-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform integer mutation

(defn uniform-integer-mutation
  "Uniformly mutates individual. For each integer in the genome, there is
   uniform-mutation-constant-tweak-rate probability of being mutated."
  [ind {:keys [uniform-mutation-constant-tweak-rate uniform-mutation-int-gaussian-standard-deviation
               maintain-ancestors atom-generators]
        :as argmap}]
  (let [uniform-mutation-constant-tweak-rate 
        (number uniform-mutation-constant-tweak-rate)
        
        uniform-mutation-int-gaussian-standard-deviation 
        (number uniform-mutation-int-gaussian-standard-deviation)
        
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (integer? const)
                               (assoc token
                                 :instruction
                                 (round (perturb-with-gaussian-noise 
                                          uniform-mutation-int-gaussian-standard-deviation const)))
                               token)))
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-constant-tweak-rate)
                          (constant-mutator token)
                          token))
        new-genome (mapv token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
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
        (number uniform-mutation-constant-tweak-rate)
        
        uniform-mutation-float-gaussian-standard-deviation
        (number uniform-mutation-float-gaussian-standard-deviation)
        
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
        (number uniform-mutation-rate)
        
        uniform-mutation-tag-gaussian-standard-deviation
        (number uniform-mutation-tag-gaussian-standard-deviation)
         
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
        (number uniform-mutation-rate)
        
        uniform-mutation-string-char-change-rate
        (number uniform-mutation-string-char-change-rate)
        
        string-tweak (fn [st]
                       (apply str (map (fn [c]
                                         (if (< (lrand) uniform-mutation-string-char-change-rate)
                                           (lrand-nth (concat ["\n" "\t"] 
                                                              (map (comp str char) (range 32 127))))
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
  (let [uniform-mutation-constant-tweak-rate (number uniform-mutation-constant-tweak-rate)
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
               epigenetic-markers maintain-ancestors]}]
  (if (not (some #{:close} epigenetic-markers))
    ind
    (let [uniform-close-mutation-rate (number uniform-close-mutation-rate)
          close-increment-rate (number close-increment-rate)
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
                       :ancestors (if maintain-ancestors
                                    (cons (:genome ind) (:ancestors ind))
                                    (:ancestors ind))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform silence mutation

(defn uniform-silence-mutation
  "Uniformly mutates the :silent's in the individual's instruction maps. Each
   :silent will have a uniform-silence-mutation-rate probability of being switched."
  [ind {:keys [uniform-silence-mutation-rate
               epigenetic-markers maintain-ancestors]}]
  (if (not (some #{:silent} epigenetic-markers))
    ind
    (let [uniform-silence-mutation-rate (number uniform-silence-mutation-rate)
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
                       :ancestors (if maintain-ancestors
                                    (cons (:genome ind) (:ancestors ind))
                                    (:ancestors ind))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform deletion

(defn uniform-deletion
  "Returns the individual with each element of its genome possibly deleted, with probability
given by uniform-deletion-rate."
  [ind {:keys [uniform-deletion-rate maintain-ancestors]}]
  (let [rate (number uniform-deletion-rate)
        new-genome (vec (filter identity
                                (map #(if (< (lrand) rate) nil %)
                                     (:genome ind))))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform addition

(defn uniform-addition
  "Returns the individual with each element of its genome possibly preceded or followed by
  a new gene, with probability given by uniform-addition-rate."
  [ind {:keys [uniform-addition-rate maintain-ancestors atom-generators] :as argmap}]
  (let [rate (number uniform-addition-rate)
        new-genome (vec (apply concat
                               (map #(if (< (lrand) rate)
                                      (lshuffle [% 
                                                 (random-plush-instruction-map 
                                                   atom-generators argmap)])
                                      [%])
                                    (:genome ind))))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
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
  (let [addition-rate (number uniform-addition-and-deletion-rate)
        deletion-rate (if (zero? addition-rate)
                        0
                        (/ 1 (+ (/ 1 addition-rate) 1)))
        after-addition (vec (apply concat
                                   (map #(if (< (lrand) addition-rate)
                                           (lshuffle [% 
                                                      (random-plush-instruction-map 
                                                        atom-generators argmap)])
                                           [%])
                                        (:genome ind))))
        new-genome (vec (filter identity
                                (map #(if (< (lrand) deletion-rate) nil %)
                                     after-addition)))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :age (inc (:age ind))
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform combination and deletion

(defn uniform-combination-and-deletion
  "Returns parent1 after two passes of modification. In the first pass, each element 
  of its genome may possibly be preceded or followed by the corresponding element from
  parent 2's genome (which will wrap if it is too short). In the second pass, each
  element of the genome may possibly be deleted. Probabilities are given by 
  uniform-combination-and-deletion-rate."
  [parent1 parent2 
   {:keys [uniform-combination-and-deletion-rate maintain-ancestors atom-generators] 
    :as argmap}]
  (let [combination-rate (number uniform-combination-and-deletion-rate)
        deletion-rate (if (zero? combination-rate)
                        0
                        (/ 1 (+ (/ 1 combination-rate) 1)))
        after-combination (vec 
                            (apply concat
                                   (map (fn [g1 g2]
                                          (if (< (lrand) combination-rate)
                                            (lshuffle [g1 g2])
                                            [g1]))
                                        (:genome parent1)
                                        (cycle (:genome parent2)))))
        new-genome (vec (filter identity
                                (map #(if (< (lrand) deletion-rate) nil %)
                                     after-combination)))]
    (make-individual :genome new-genome
                     :history (:history parent1)
                     :age ((age-combining-function argmap) 
                           (inc (:age parent1)) 
                           (inc (:age parent2)))
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
  (let [alternation-rate (number alternation-rate)
        alignment-deviation (number alignment-deviation)
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
                     :age ((age-combining-function argmap) 
                           (inc (:age parent1)) 
                           (inc (:age parent2)))
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
                     :age ((age-combining-function argmap) 
                           (inc (:age parent1)) 
                           (inc (:age parent2)))
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
          short-genome-lengthened (concat short-genome
                                          (repeat (- (count long-genome) (count short-genome))
                                                  'uniform-padding))
          new-genome (vec (remove-uniform-padding
                            (map (fn [x1 x2]
                                   (if (< (lrand) 0.5)
                                     x1
                                     x2))
                                 short-genome-lengthened
                                 long-genome)))]
      (make-individual :genome new-genome
                       :history (:history parent1)
                       :age ((age-combining-function argmap) 
                             (inc (:age parent1)) 
                             (inc (:age parent2)))
                       :ancestors (if maintain-ancestors
                                    (cons (:genome parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoconstuction

;; NOTE: EXPERIMENTAL!

(defn process-genome-for-autoconstruction
  "Replaces input instructions with noops and, if deterministic? is true then
replaces non-autoconstructive rand instructions with noops, while if deterministic? 
is false replaces autoconstructive_<type>_rand with <type>_rand."
  [genome deterministic?]
  (let [input-instruction? (fn [instruction]
                             (and (symbol? instruction)
                                  (or (re-seq #"in\d+" (name instruction)) ;; from input-output
                                      (re-seq #"in_dm" (name instruction)) ;; from digital-multiplier
                                      (some #{instruction}
                                            '(a0 a1 a2 d0 d1 d2 d3 d4 d5 d6 d7)))))] ;; from mux problems
    (mapv (fn [instruction-map]
            (if (input-instruction? (:instruction instruction-map))
              (assoc instruction-map :instruction 'code_noop)
              (if deterministic?
                (if (some #{(:instruction instruction-map)}
                           '(boolean_rand integer_rand float_rand code_rand
                                          string_rand char_rand 
                                          genome_gene_randomize))
                  (assoc instruction-map :instruction 'code_noop)
                  instruction-map)
                (if (= (:instruction instruction-map) 'autoconstructive_integer_rand)
                  (assoc instruction-map :instruction 'integer_rand)
                  (if (= (:instruction instruction-map) 'autoconstructive_boolean_rand)
                    (assoc instruction-map :instruction 'boolean_rand)
                    instruction-map)))))
         genome)))

(defn produce-child-genome-by-autoconstruction
  "Runs the program expressed by parent1-genome with both parent genomes
on the genome stack and also available via input instructions, and returns
the resulting top genome."
  [parent1-genome parent2-genome deterministic? argmap]
  (let [run-result (top-item :genome
                             (run-push
                               (translate-plush-genome-to-push-program
                                 {:genome (process-genome-for-autoconstruction
                                            parent1-genome
                                            deterministic?)}
                                 argmap)
                               (-> (->> (make-push-state)
                                     (push-item parent2-genome :genome)
                                     (push-item parent1-genome :genome))
                                 (assoc :parent1-genome parent1-genome)
                                 (assoc :parent2-genome parent2-genome))))]
    (if (or (seq? run-result) (vector? run-result))
      (vec run-result)
      [])))

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
  "Returns true iff genome g passes the diversification test."
  [g argmap]
  (let [delta #(expressed-difference 
                 g
                 (produce-child-genome-by-autoconstruction g g false argmap)
                 argmap)
        diffs (repeatedly 2 delta)]
    (and (> (reduce min diffs) 0) ;; diversification threshold set here
         (> (count (distinct diffs)) 1))))

(defn no-clones-diversifying?
  "Returns true iff genome g passes the diversification test."
  [g argmap]
  (not= (translate-plush-genome-to-push-program 
          {:genome g} 
          argmap)
        (translate-plush-genome-to-push-program 
          {:genome (produce-child-genome-by-autoconstruction g g false argmap)} 
          argmap)))

(defn three-gens-diff-diffs-diversifying?
  "Returns true iff genome g passes the diversification test."
  [g argmap]
  (let [make-child #(produce-child-genome-by-autoconstruction % % false argmap)
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
    (and (> (reduce min diffs) 0)
         (apply distinct? diffs))))

(defn three-gens-some-diff-diffs-diversifying?
  "Returns true iff genome g passes the diversification test."
  [g argmap]
  (let [make-child #(produce-child-genome-by-autoconstruction % % false argmap)
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
    (and (> (reduce min diffs) 0)
         (not= c1-diff c2-diff)
         (not= c1-diff gc1-diff)
         (not= c2-diff gc2-diff))))

(defn size-and-instruction-diversifying?
  "Returns true iff genome g passes the diversification test."
  [g argmap]
  (let [numkids (:autoconstructive-si-children argmap)
        kids (repeatedly numkids 
                         #(produce-child-genome-by-autoconstruction g g false argmap))
        kid-counts (map count kids)
        instruction-set (fn [genome]
                          (hash-set (keys (frequencies (map :instruction genome)))))
        kid-sets (map instruction-set kids)]
    (and (not (some #{(count g)} kid-counts))
         (not (some #{(instruction-set g)} kid-sets))
         (or (< numkids 2) (not (apply = kid-counts)))
         (or (< numkids 2) (not (apply = kid-sets))))))

(defn three-gens-size-and-instruction-diversifying?
  "Returns true iff genome g passes the diversification test."
  [g argmap]
  (let [numkids (:autoconstructive-si-children argmap)
        make-child #(produce-child-genome-by-autoconstruction % % false argmap)
        instruction-set (fn [genome]
                          (hash-set (keys (frequencies (map :instruction genome)))))
        kids (repeatedly numkids #(make-child g))
        kid-counts (map count kids)
        kid-sets (map instruction-set kids)
        grandkids (map make-child kids)
        grandkid-counts (map count grandkids)
        grandkid-sets (map instruction-set grandkids)]
    (and (not (some #{(count g)} kid-counts))
         (not (some #{(instruction-set g)} kid-sets))
         (not (some #{(count g)} grandkid-counts))
         (not (some #{(instruction-set g)} grandkid-sets))
         (or (< numkids 2) (not (apply = (map count kids))))
         (or (< numkids 2) (not (apply = (map count grandkids))))
         (or (< numkids 2) (not (apply = (map instruction-set kids))))
         (or (< numkids 2) (not (apply = (map instruction-set grandkids)))))))

(defn safe-t-test ;; safely answers 1.0 if t-test would divide by zero
  [xvec yvec]
  (if (or (apply = xvec)
          (apply = yvec))
    1.0
    (let [new-t (TTest.)]
      (.tTest new-t (double-array xvec) (double-array yvec)))))

(defn diffmeans-diversifying?
  "Returns true iff genome g passes the diversification test."
  [g argmap]
  (let [num-children (:autoconstructive-diffmeans-children argmap)
        make-child #(produce-child-genome-by-autoconstruction % % false argmap)
        c1 (make-child g)
        diffs1 (vec (repeatedly 
                      num-children 
                      #(expressed-difference c1 (make-child c1) argmap)))]
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
          true)))))

(defn diversifying?
  "Returns true iff genome g passes the diversification test."
  [g argmap]
  ((case (:autoconstructive-diversification-test argmap)
     :gecco2016 gecco2016-diversifying?
     :three-gens-diff-diffs three-gens-diff-diffs-diversifying?
     :three-gens-some-diff-diffs three-gens-some-diff-diffs-diversifying?
     :size-and-instruction size-and-instruction-diversifying?
     :three-gens-size-and-instruction three-gens-size-and-instruction-diversifying?
     :diffmeans diffmeans-diversifying?
     :no-clones no-clones-diversifying?
     :none (fn [genome argmap] true))
    g
    argmap))

(defn fotd-autoconstruction
  "The current 'flavor of the day' version of autoconstruction, used by specifying
  :autoconstructive-fotd true in the arguments to pushgp. Expect changes. Other
  autoconstruction-related parameters may or may not have any effect when using the
  fotd."
  [parent1 parent2 {:keys [maintain-ancestors atom-generators max-genome-size-in-initial-program 
                           error-function]
                    :as argmap}]
  (let [parent1-genome (:genome parent1)
        parent2-genome (:genome parent2)
        parental-errors? (fn [errors]
                           (some #{errors} [(:errors parent1) (:errors parent2)]))
        make-child-genome (fn [g1 g2] 
                            (produce-child-genome-by-autoconstruction g1 g2 false argmap))
        diff #(expressed-difference %1 %2 argmap)
        genome-error #(do (swap! evaluations-count inc)
                        (error-function (translate-plush-genome-to-push-program 
                                          {:genome %} 
                                          argmap)))
        acceptable? (fn [g]
                      (let [c1 (make-child-genome g g)
                            c1-diff (diff g c1)]
                        (if (zero? c1-diff)
                          false
                          (let [c1-errors (genome-error c1)]
                            (if (parental-errors? c1-errors)
                              false
                              (let [c2 (make-child-genome g g)
                                    c2-diff (diff g c2)]
                                (if (or (zero? c2-diff)
                                        (= c1-diff c2-diff))
                                  false
                                  (let [c2-errors (genome-error c2)]
                                    (if (or (parental-errors? c2-errors)
                                            (= c1-errors c2-errors))
                                      false
                                      (let [gc1-diff (diff c1 (make-child-genome c1 c1))]
                                        (if (or (zero? gc1-diff)
                                                (= c1-diff gc1-diff))
                                          false
                                          (let [gc2-diff (diff c2 (make-child-genome c2 c2))]
                                            (if (or (zero? gc2-diff)
                                                    (= c2-diff gc2-diff))
                                              false
                                              true)))))))))))))
        child-genome (make-child-genome parent1-genome parent2-genome)
        new-genome (if (acceptable? child-genome)
                     child-genome
                     (let [replacement (random-plush-genome max-genome-size-in-initial-program 
                                                            atom-generators 
                                                            argmap)]
                       (if (acceptable? replacement)
                         replacement
                         [])))]
    (assoc (make-individual :genome new-genome
                            :history (:history parent1)
                            :age ((age-combining-function argmap) 
                                  (inc (:age parent1)) 
                                  (inc (:age parent2)))
                            :ancestors (if maintain-ancestors
                                         (cons (:genome parent1) (:ancestors parent1))
                                         (:ancestors parent1)))
      :is-random-replacement (not= child-genome new-genome))))

(defn autoconstruction
  "Returns a genome for a child produced either by autoconstruction (executing parent1
with both parents on top of the genome stack and also available via input instructions)
or by cloning. In either case if the child is not diversifying then a random
genome is returned instead IF that is itself diversifying; if it isn't then an empty 
genome is returned. The construct/clone ration is hardcoded here, but might
be set globally or eliminated in the future."
  [parent1 parent2 {:keys [maintain-ancestors atom-generators max-genome-size-in-initial-program 
                           error-function autoconstructive-improve-or-diversify 
                           autoconstructive-fotd autoconstructive-clone-probability
                           autoconstructive-entropy]
                    :as argmap}]
  (if autoconstructive-fotd
    (fotd-autoconstruction parent1 parent2 argmap)
    (let [parent1-genome (:genome parent1)
          parent2-genome (:genome parent2)
          pre-entropy-child-genome (if (> (lrand) autoconstructive-clone-probability)
                                     (produce-child-genome-by-autoconstruction 
                                       parent1-genome parent2-genome false argmap)
                                     parent1-genome)
          child-genome (if (zero? autoconstructive-entropy)
                         pre-entropy-child-genome
                         (vec (filter identity
                                      (map #(if (< (lrand) autoconstructive-entropy) nil %)
                                           pre-entropy-child-genome))))
          child-errors (if autoconstructive-improve-or-diversify
                         (do
                           (swap! evaluations-count inc)
                           (error-function (translate-plush-genome-to-push-program 
                                             {:genome child-genome} 
                                             argmap)))
                         nil)
          variant (diversifying? child-genome argmap)
          use-child (or variant
                        (and autoconstructive-improve-or-diversify
                             (some (fn [[child-error parent1-error parent2-error]]
                                     (< child-error (min parent1-error parent2-error)))
                                   (mapv vector child-errors (:errors parent1) (:errors parent2)))))
          new-genome (if use-child
                       child-genome
                       (random-plush-genome max-genome-size-in-initial-program atom-generators argmap))]
      (assoc (make-individual :genome (if (or use-child (diversifying? new-genome argmap))
                                        new-genome
                                        [])
                              :history (:history parent1)
                              :age ((age-combining-function argmap) 
                                    (inc (:age parent1)) 
                                    (inc (:age parent2)))
                              :ancestors (if maintain-ancestors
                                           (cons (:genome parent1) (:ancestors parent1))
                                           (:ancestors parent1)))
        :is-random-replacement
        (if use-child false true)))))













