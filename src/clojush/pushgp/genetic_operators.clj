(ns clojush.pushgp.genetic-operators
  (:use [clojush util random individual globals interpreter translate pushstate]
        clojush.instructions.tag
        [clojure.math.numeric-tower])
  (:require [clojure.string :as string]
            [incanter.stats :as stats]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reproduction

(defn reproduction
  "Returns parent"
  [ind argmap]
  ind)

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
        new-tag-num (mod (round (perturb-with-gaussian-noise uniform-mutation-tag-gaussian-standard-deviation tag-num))
                         @global-tag-limit)
        new-instr (symbol (apply str (interpose  "_" (concat (butlast tagparts) (list (str new-tag-num))))))]
    (assoc instr-map :instruction new-instr)))

(defn uniform-mutation
  "Uniformly mutates individual. For each token in program, there is
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
  (let [string-tweak (fn [st]
                       (apply str (map (fn [c]
                                         (if (< (lrand) uniform-mutation-string-char-change-rate)
                                           (lrand-nth (concat ["\n" "\t"] (map (comp str char) (range 32 127))))
                                           c))
                                       st)))
        instruction-mutator (fn [token]
                              (assoc token
                                     :instruction
                                     (:instruction (first (random-plush-genome 1 atom-generators argmap)))))
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (tag-instruction? const)
                               (tag-gaussian-tweak token uniform-mutation-tag-gaussian-standard-deviation)
                               (assoc token
                                      :instruction
                                      (cond
                                        (float? const) (perturb-with-gaussian-noise uniform-mutation-float-gaussian-standard-deviation const)
                                        (integer? const) (round (perturb-with-gaussian-noise uniform-mutation-int-gaussian-standard-deviation const))
                                        (string? const) (string-tweak const)
                                        (or (= const true) (= const false)) (lrand-nth [true false])
                                        :else (:instruction (first (random-plush-genome 1 atom-generators argmap))))))))               
        token-mutator (fn [token]
                        (if (< (lrand) uniform-mutation-rate)
                          (if (< (lrand) uniform-mutation-constant-tweak-rate)
                            (constant-mutator token)
                            (instruction-mutator token))
                          token))
        new-genome (map token-mutator (:genome ind))]
    (make-individual :genome new-genome
                     :history (:history ind)
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
    (let [close-mutator (fn [instr-map]
                          (let [closes (get instr-map :close 0)]
                            (assoc instr-map :close
                                   (if (< (lrand) uniform-close-mutation-rate)
                                     (if (< (lrand) close-increment-rate) ;Rate at which to increase closes instead of decrease
                                       (inc closes)
                                       (if (<= closes 0)
                                         0
                                         (dec closes)))
                                     closes))))
          new-genome (map close-mutator (:genome ind))]
      (make-individual :genome new-genome
                       :history (:history ind)
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
    (let [silent-mutator (fn [instr-map]
                           (let [silent (get instr-map :silent false)]
                             (assoc instr-map :silent
                                    (if (< (lrand) uniform-silence-mutation-rate)
                                      (not silent)
                                      silent))))
          new-genome (map silent-mutator (:genome ind))]
      (make-individual :genome new-genome
                       :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:genome ind) (:ancestors ind))
                                    (:ancestors ind))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform deletion

(defn uniform-deletion
  "Returns the individual with each element of its genome possibly deleted, with probability
given by uniform-deletion-rate."
  [ind {:keys [uniform-deletion-rate maintain-ancestors]}]
  (let [new-genome (filter identity 
                           (map #(if (< (lrand) uniform-deletion-rate) % nil)
                                (:genome ind)))]
    (make-individual :genome new-genome
                     :history (:history ind)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome ind) (:ancestors ind))
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alternation

(defn alternation
  "Uniformly alternates between the two parents using a similar method to that
   used in ULTRA."
  [parent1 parent2 {:keys [alternation-rate alignment-deviation
                           max-points maintain-ancestors] :as argmap}]
  (let [s1 (:genome parent1)
        s2 (:genome parent2)
        new-genome (loop [i 0
                          use-s1 (lrand-nth [true false])
                          result-genome []]
                     (if (or (>= i (count (if use-s1 s1 s2))) ;; finished current program
                             (> (count result-genome) (* 2 max-points))) ;; runaway growth
                       (seq result-genome);; Return, converting back into a sequence
                       (if (< (lrand) alternation-rate)
                         (recur (max 0 (+' i (Math/round (*' alignment-deviation (gaussian-noise-factor)))))
                                (not use-s1)
                                result-genome)
                         (recur (inc i)
                                use-s1
                                (conj result-genome (nth (if use-s1 s1 s2) i))))))]
    (make-individual :genome new-genome
                     :history (:history parent1)
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
        new-genome (concat (take (first p1-points) genome1)
                           (drop (first p2-points) (take (second p2-points) genome2))
                           (drop (second p1-points) genome1))]
    (make-individual :genome new-genome
                     :history (:history parent1)
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
          new-genome (remove-uniform-padding
                       (map (fn [x1 x2]
                              (if (< (lrand) 0.5)
                                x1
                                x2))
                            short-genome-lengthened
                            long-genome))]
      (make-individual :genome new-genome
                       :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:genome parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoconstuction

;; NOTE: EXPERIMENTAL!

(define-registered
  autoconstructive_integer_rand ;; replace with integer_rand during construction; noop otherwise
  ^{:stack-types [:exec]} (fn [state] state))

(defn process-genome-for-autoconstruction
  "Replace input instructions with noops and replace autoconstructive_integer_rand with integer_rand."
  [genome deterministic?]
  (let [input-instruction? (fn [instruction]
                             (and (symbol? instruction) 
                                  (or (re-seq #"in\d+" (name instruction)) ;; from input-output
                                      (re-seq #"in_dm" (name instruction)))))] ;; from digital-multiplier
    (map (fn [instruction-map]
           (if (input-instruction? (:instruction instruction-map))
             (assoc instruction-map :instruction 'code_noop)
             (if (and (not deterministic?)
                      (= (:instruction instruction-map) 'autoconstructive_integer_rand))
               (assoc instruction-map :instruction 'integer_rand)
               instruction-map)))
         genome)))

(defn produce-child-genome-by-autoconstruction
  [parent1-genome parent2-genome deterministic?]
  (let [run-result (top-item :genome
                             (run-push
                               (translate-plush-genome-to-push-program
                                 {:genome (process-genome-for-autoconstruction
                                            parent1-genome
                                            deterministic?)})
                               (-> (->> (make-push-state)
                                     (push-item parent2-genome :genome)
                                     (push-item parent1-genome :genome))
                                 (assoc :parent1-genome parent1-genome)
                                 (assoc :parent2-genome parent2-genome))))]
    (if (or (seq? run-result) (vector? run-result))
      run-result
      ())))

;(defn express-same-programs?
;  [g1 g2]
;  (or (= g1 g2) ;; avoid translation for equivalent genomes 
;      (= (translate-plush-genome-to-push-program {:genome g1})
;         (translate-plush-genome-to-push-program {:genome g2}))))

;(defn expressively-indistinct?
;  [genomes]
;  (or (not (distinct? genomes)) ;; avoid translation for equivalent genomes 
;      (not (distinct? (map #(translate-plush-genome-to-push-program {:genome %}) genomes)))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (expressively-indistinct? [c1 c2])
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g true)
;            c2c (produce-child-genome-by-autoconstruction c2 g true)]
;        (if (expressively-indistinct? [c1 c2 c1c c2c])
;          false
;          (let [c1cc (produce-child-genome-by-autoconstruction c1c g true)
;                c2cc (produce-child-genome-by-autoconstruction c2c g true)]
;            (if (expressively-indistinct? [c1 c2 c1c c2c c1cc c2cc])
;              false
;              (if (let [g-size (count g)
;                        descendant-sizes (map count [c1 c2 c1c c2c c1cc c2cc])]
;                   (or (>= (apply min descendant-sizes)
;                           g-size)
;                       (<= (apply max descendant-sizes)
;                           g-size)))
;                false
;                true))))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (expressively-indistinct? [c1 c2])
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g true)
;            c2c (produce-child-genome-by-autoconstruction c2 g true)]
;        (if (expressively-indistinct? [c1 c2 c1c c2c])
;          false
;          (if (= (levenshtein-distance c1 c1c)
;                 (levenshtein-distance c2 c2c))
;            false
;            true))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (expressively-indistinct? [c1 c2])
;      false
;      (if (= (levenshtein-distance g c1)
;             (levenshtein-distance g c2))
;        false
;        true))))

(defn expressed-program-sequence-from-genome
  [g]
  (ensure-list
    (list-to-open-close-sequence 
      (translate-plush-genome-to-push-program {:genome g}))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        g-seq (expressed-program-sequence-from-genome g)
;        c1-seq (expressed-program-sequence-from-genome c1)
;        c2-seq (expressed-program-sequence-from-genome c2)]
;    (if (not (distinct? 0
;                        (levenshtein-distance g-seq c1-seq)
;                        (levenshtein-distance g-seq c2-seq)))
;      false
;      (let [c1c1 (produce-child-genome-by-autoconstruction c1 g false)
;            c1c2 (produce-child-genome-by-autoconstruction c1 g false)
;            c1c1-seq (expressed-program-sequence-from-genome c1c1)
;            c1c2-seq (expressed-program-sequence-from-genome c1c2)]
;        (if (not (distinct? 0
;                            (levenshtein-distance g-seq c1-seq)
;                            (levenshtein-distance c1-seq c1c1-seq)
;                            (levenshtein-distance c1-seq c1c2-seq)))
;          false
;          (let [c2c1 (produce-child-genome-by-autoconstruction c2 g false)
;                c2c2 (produce-child-genome-by-autoconstruction c2 g false)
;                c2c1-seq (expressed-program-sequence-from-genome c2c1)
;                c2c2-seq (expressed-program-sequence-from-genome c2c2)]
;            (if (not (distinct? 0
;                                (levenshtein-distance g-seq c2-seq)
;                                (levenshtein-distance c2-seq c2c1-seq)
;                                (levenshtein-distance c2-seq c2c2-seq)))
;              false
;              true)))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        g-seq (expressed-program-sequence-from-genome g)
;        c1-seq (expressed-program-sequence-from-genome c1)
;        c2-seq (expressed-program-sequence-from-genome c2)]
;    (distinct? 0
;               (levenshtein-distance g-seq c1-seq)
;               (levenshtein-distance g-seq c2-seq))))

(defn expressively-indistinct?
  [genomes]
  (or (not (apply distinct? genomes)) ;; avoid translation for equivalent genomes 
      (not (apply distinct? (map #(translate-plush-genome-to-push-program {:genome %}) genomes)))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (expressively-indistinct? [c1 c2])
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g true)
;            c2c (produce-child-genome-by-autoconstruction c2 g true)]
;        (if (expressively-indistinct? [c1 c2 c1c c2c])
;          false
;          (let [c1cc (produce-child-genome-by-autoconstruction c1c g true)
;                c2cc (produce-child-genome-by-autoconstruction c2c g true)]
;            (if (expressively-indistinct? [c1 c2 c1c c2c c1cc c2cc])
;              false
;              #_(if (let [g-size (count g)
;                         descendant-sizes (map count [c1 c2 c1c c2c c1cc c2cc])]
;                    (or (>= (apply min descendant-sizes)
;                            g-size)
;                        (<= (apply max descendant-sizes)
;                            g-size)))
;                 false
;                 true)
;              true)))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c1c (produce-child-genome-by-autoconstruction c1 g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c2c (produce-child-genome-by-autoconstruction c2 g false)
;        c1-seq (expressed-program-sequence-from-genome c1)
;        c2-seq (expressed-program-sequence-from-genome c2)
;        c1c-seq (expressed-program-sequence-from-genome c1c)
;        c2c-seq (expressed-program-sequence-from-genome c2c)]
;    (distinct? 0
;               (levenshtein-distance c1-seq c1c-seq)
;               (levenshtein-distance c2-seq c2c-seq))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c1-seq (expressed-program-sequence-from-genome c1)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c2-seq (expressed-program-sequence-from-genome c2)]
;    (if (not (distinct? c1-seq c2-seq))
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g false)
;            c1c-seq (expressed-program-sequence-from-genome c1c)
;            c2c (produce-child-genome-by-autoconstruction c2 g false)
;            c2c-seq (expressed-program-sequence-from-genome c2c)]
;        (and (distinct? c1-seq c2-seq c1c-seq c2c-seq)
;             (distinct? 0
;                        (levenshtein-distance c1-seq c1c-seq)
;                        (levenshtein-distance c2-seq c2c-seq)))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c1-seq (expressed-program-sequence-from-genome c1)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c2-seq (expressed-program-sequence-from-genome c2)]
;    (if (not (distinct? c1-seq c2-seq))
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g true)
;            c1c-seq (expressed-program-sequence-from-genome c1c)
;            c2c (produce-child-genome-by-autoconstruction c2 g true)
;            c2c-seq (expressed-program-sequence-from-genome c2c)]
;        (and (distinct? c1-seq c2-seq c1c-seq c2c-seq)
;             (distinct? 0
;                        (levenshtein-distance c1-seq c1c-seq)
;                        (levenshtein-distance c2-seq c2c-seq)))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (expressively-indistinct? [c1 c2])
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g true)
;            c2c (produce-child-genome-by-autoconstruction c2 g true)]
;        (if (expressively-indistinct? [c1 c2 c1c c2c])
;          false
;          (let [c1cc (produce-child-genome-by-autoconstruction c1c g true)
;                c2cc (produce-child-genome-by-autoconstruction c2c g true)]
;            (if (expressively-indistinct? [c1 c2 c1c c2c c1cc c2cc])
;              false
;              (distinct? 0
;                         (levenshtein-distance 
;                           (expressed-program-sequence-from-genome c1c)
;                           (expressed-program-sequence-from-genome c1cc))
;                         (levenshtein-distance 
;                           (expressed-program-sequence-from-genome c2c)
;                           (expressed-program-sequence-from-genome c2cc))))))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (expressively-indistinct? [c1 c2])
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g true)
;            c2c (produce-child-genome-by-autoconstruction c2 g true)]
;        (if (or (expressively-indistinct? [c1 c2 c1c c2c])
;                (not (distinct?
;                       (levenshtein-distance 
;                         (expressed-program-sequence-from-genome c1)
;                         (expressed-program-sequence-from-genome c1c))
;                       (levenshtein-distance 
;                         (expressed-program-sequence-from-genome c2)
;                         (expressed-program-sequence-from-genome c2c)))))
;          false
;          (let [c1cc (produce-child-genome-by-autoconstruction c1c g true)
;                c2cc (produce-child-genome-by-autoconstruction c2c g true)]
;            (not (expressively-indistinct? [c1 c2 c1c c2c c1cc c2cc]))))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (not (distinct?
;               0
;               (levenshtein-distance 
;                 (expressed-program-sequence-from-genome g)
;                 (expressed-program-sequence-from-genome c1))
;               (levenshtein-distance 
;                 (expressed-program-sequence-from-genome g)
;                 (expressed-program-sequence-from-genome c2))))
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g true)
;            c2c (produce-child-genome-by-autoconstruction c2 g true)]
;        (if (expressively-indistinct? [c1 c2 c1c c2c])             
;          false
;          (let [c1cc (produce-child-genome-by-autoconstruction c1c g true)
;                c2cc (produce-child-genome-by-autoconstruction c2c g true)]
;            (not (expressively-indistinct? [c1 c2 c1c c2c c1cc c2cc]))))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (expressively-indistinct? [c1 c2])
;      false
;      (let [c1c (produce-child-genome-by-autoconstruction c1 g true)
;            c2c (produce-child-genome-by-autoconstruction c2 g true)]
;        (if (expressively-indistinct? [c1 c2 c1c c2c])             
;          false
;          (let [c1cc (produce-child-genome-by-autoconstruction c1c g true)
;                c2cc (produce-child-genome-by-autoconstruction c2c g true)]
;            (not (expressively-indistinct? [c1 c2 c1c c2c c1cc c2cc]))))))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c1c (produce-child-genome-by-autoconstruction c1 g false)
;        c2c (produce-child-genome-by-autoconstruction c2 g false)]
;    (distinct?
;      0
;      (levenshtein-distance 
;        (expressed-program-sequence-from-genome c1)
;        (expressed-program-sequence-from-genome c1c))
;      (levenshtein-distance 
;        (expressed-program-sequence-from-genome c2)
;        (expressed-program-sequence-from-genome c2c)))))

(defn translate
  [g]
  (translate-plush-genome-to-push-program {:genome g}))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c1c (produce-child-genome-by-autoconstruction c1 g false)
;        c2c (produce-child-genome-by-autoconstruction c2 g false)]
;    (and (apply distinct? (map translate [g c1 c2 c1c c2c]))
;         (distinct?
;           0
;           (levenshtein-distance 
;             (expressed-program-sequence-from-genome c1)
;             (expressed-program-sequence-from-genome c1c))
;           (levenshtein-distance 
;             (expressed-program-sequence-from-genome c2)
;             (expressed-program-sequence-from-genome c2c))))))

(defn expressed-difference
  [g1 g2]
  (levenshtein-distance (expressed-program-sequence-from-genome g1)
                        (expressed-program-sequence-from-genome g2)))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c1c1 (produce-child-genome-by-autoconstruction c1 g false)
;        c1c2 (produce-child-genome-by-autoconstruction c1 g false)
;        c2c1 (produce-child-genome-by-autoconstruction c2 g false)
;        c2c2 (produce-child-genome-by-autoconstruction c2 g false)]
;    (and (apply distinct? (map translate [g c1 c2 c1c1 c1c2 c2c1 c2c2]))
;         (= (expressed-difference g c1)
;            (expressed-difference g c2))
;         (= (expressed-difference c1 c1c1)
;            (expressed-difference c1 c1c2))
;         (= (expressed-difference c2 c2c1)
;            (expressed-difference c2 c2c2))
;         (distinct?
;           0
;           (expressed-difference g c1)
;           (expressed-difference c1 c1c1)
;           (expressed-difference c1 c2c1)))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c1c (produce-child-genome-by-autoconstruction c1 g false)
;        c2c (produce-child-genome-by-autoconstruction c2 g false)]
;    (distinct?
;      0
;      (expressed-difference g c1)
;      (expressed-difference g c2)
;      (expressed-difference c1 c1c)
;      (expressed-difference c2 c2c))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c1c1 (produce-child-genome-by-autoconstruction c1 g false)
;        c1c2 (produce-child-genome-by-autoconstruction c1 g false)
;        c2c1 (produce-child-genome-by-autoconstruction c2 g false)
;        c2c2 (produce-child-genome-by-autoconstruction c2 g false)]
;    (distinct?
;      0
;      (expressed-difference g c1)
;      (expressed-difference g c2)
;      (expressed-difference c1 c1c1)
;      (expressed-difference c1 c1c2)
;      (expressed-difference c2 c2c1)
;      (expressed-difference c2 c2c2))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c1-diff (expressed-difference g c1)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c2-diff (expressed-difference g c2)
;        c1c1 (produce-child-genome-by-autoconstruction c1 g false)
;        c1c1-diff (expressed-difference c1 c1c1)
;        c1c2 (produce-child-genome-by-autoconstruction c1 g false)
;        c1c2-diff (expressed-difference c1 c1c2)
;        c2c1 (produce-child-genome-by-autoconstruction c2 g false)
;        c2c1-diff (expressed-difference c2 c2c1)
;        c2c2 (produce-child-genome-by-autoconstruction c2 g false)
;        c2c2-diff (expressed-difference c2 c2c2)]
;    (and (apply distinct? (map translate [g c1 c2 c1c1 c1c2 c2c1 c2c2]))
;         (distinct? 0 c1-diff c1c1-diff)
;         (distinct? 0 c1-diff c1c2-diff)
;         (distinct? 0 c2-diff c2c1-diff)
;         (distinct? 0 c2-diff c2c2-diff))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c1-diff (expressed-difference g c1)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c2-diff (expressed-difference g c2)
;        c1c1 (produce-child-genome-by-autoconstruction c1 c1 false)
;        c1c1-diff (expressed-difference c1 c1c1)
;        c1c2 (produce-child-genome-by-autoconstruction c1 c1 false)
;        c1c2-diff (expressed-difference c1 c1c2)
;        c2c1 (produce-child-genome-by-autoconstruction c2 c2 false)
;        c2c1-diff (expressed-difference c2 c2c1)
;        c2c2 (produce-child-genome-by-autoconstruction c2 c2 false)
;        c2c2-diff (expressed-difference c2 c2c2)]
;    (and (apply distinct? (map translate [g c1 c2 c1c1 c1c2 c2c1 c2c2]))
;         (distinct? 0 c1-diff c1c1-diff)
;         (distinct? 0 c1-diff c1c2-diff)
;         (distinct? 0 c2-diff c2c1-diff)
;         (distinct? 0 c2-diff c2c2-diff))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c1-diff (expressed-difference g c1)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c2-diff (expressed-difference g c2)
;        c1c1 (produce-child-genome-by-autoconstruction c1 c1 false)
;        c1c1-diff (expressed-difference c1 c1c1)
;        c1c1-total-diff (expressed-difference g c1c1)
;        c1c2 (produce-child-genome-by-autoconstruction c1 c1 false)
;        c1c2-diff (expressed-difference c1 c1c2)
;        c1c2-total-diff (expressed-difference g c1c2)
;        c2c1 (produce-child-genome-by-autoconstruction c2 c2 false)
;        c2c1-diff (expressed-difference c2 c2c1)
;        c2c1-total-diff (expressed-difference g c2c1)
;        c2c2 (produce-child-genome-by-autoconstruction c2 c2 false)
;        c2c2-diff (expressed-difference c2 c2c2)
;        c2c2-total-diff (expressed-difference c2 c2c2)]
;    (and (apply distinct? (map translate [g c1 c2 c1c1 c1c2 c2c1 c2c2]))
;         (distinct? 0 c1-diff c1c1-diff)
;         (distinct? 0 c1-diff c1c2-diff)
;         (distinct? 0 c2-diff c2c1-diff)
;         (distinct? 0 c2-diff c2c2-diff)
;         (< 0 c1-diff c1c1-total-diff)
;         (< 0 c1-diff c1c2-total-diff)
;         (< 0 c2-diff c2c1-total-diff)
;         (< 0 c2-diff c2c2-total-diff)
;         )))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c1-diff (expressed-difference g c1)
;        c1c (produce-child-genome-by-autoconstruction c1 c1 true)
;        c1c-diff (expressed-difference c1 c1c)
;        c1cc (produce-child-genome-by-autoconstruction c1c c1c true)
;        c1cc-diff (expressed-difference c1c c1cc)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c2-diff (expressed-difference g c2)
;        c2c (produce-child-genome-by-autoconstruction c2 c2 true)
;        c2c-diff (expressed-difference c2 c2c)
;        c2cc (produce-child-genome-by-autoconstruction c2c c2c true)
;        c2cc-diff (expressed-difference c2c c2cc)]
;    (and (apply distinct? (map translate [g c1 c1c c1cc c2 c2c c2cc]))
;         (distinct? 0 (- c1c-diff c1-diff) (- c1cc-diff c1c-diff))
;         (distinct? 0 (- c2c-diff c2-diff) (- c2cc-diff c2c-diff)))))

;(defn reproductively-competent?
;  [g]
;  (let [c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)
;        c3 (produce-child-genome-by-autoconstruction g g true)
;        c4 (produce-child-genome-by-autoconstruction g c1 true)]
;    (apply distinct? (map translate [g c1 c2 c3 c4]))))

;(defn reproductively-competent?
;  [g]
;  (let [num-kids 20
;        c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (not (apply distinct? (map translate [g c1 c2])))
;      false
;      (let [c1-child-diffs (mapv #(expressed-difference c1 %)
;                                 (loop [kids []]
;                                   (if (>= (count kids) num-kids)
;                                     kids
;                                     (recur (conj kids 
;                                                  (produce-child-genome-by-autoconstruction c1 c1 false))))))
;            c2-child-diffs (mapv #(expressed-difference c2 %)
;                                 (loop [kids []]
;                                   (if (>= (count kids) num-kids)
;                                     kids
;                                     (recur (conj kids 
;                                                  (produce-child-genome-by-autoconstruction c2 c2 false))))))]
;        (< (if (and (apply = c1-child-diffs)
;                    (apply = c2-child-diffs)) ;; this case will cause t-test to error
;             (if (= (first c1-child-diffs) (first c2-child-diffs))
;               1.0
;               0.0)
;             (:p-value (stats/t-test c1-child-diffs :y c2-child-diffs)))
;           0.1)))))

;(defn reproductively-competent?
;  [g]
;  (let [num-kids 20
;        c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (not (apply distinct? (map translate [g c1 c2])))
;      false
;      (let [c1-kids (for [_ (range num-kids)]
;                      (produce-child-genome-by-autoconstruction c1 c1 false))
;            c2-kids (for [_ (range num-kids)]
;                      (produce-child-genome-by-autoconstruction c2 c2 false))]
;        (if (not (apply distinct? (map translate (concat c1-kids c2-kids [g c1 c2]))))
;          false
;          (let [c1-kid-diffs (mapv #(expressed-difference c1 %) c1-kids)
;                c2-kid-diffs (mapv #(expressed-difference c2 %) c2-kids)]
;        (< (if (and (apply = c1-kid-diffs)
;                    (apply = c2-kid-diffs)) ;; this case will cause t-test to error
;             (if (= (first c1-kid-diffs) (first c2-kid-diffs))
;               1.0
;               0.0)
;             (:p-value (stats/t-test c1-kid-diffs :y c2-kid-diffs)))
;           0.01)))))))

;(defn reproductively-competent?
;  [g]
;  (let [num-kids 20
;        c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (not (apply distinct? (map translate [g c1 c2])))
;      false
;      (let [c1-kids (for [_ (range num-kids)]
;                      (produce-child-genome-by-autoconstruction c1 g false))
;            c2-kids (for [_ (range num-kids)]
;                      (produce-child-genome-by-autoconstruction c2 g false))]
;        (if (not (apply distinct? (map translate (concat c1-kids c2-kids [g c1 c2]))))
;          false
;          (let [c1-kid-diffs (mapv #(expressed-difference c1 %) c1-kids)
;                c2-kid-diffs (mapv #(expressed-difference c2 %) c2-kids)]
;        (< (if (and (apply = c1-kid-diffs)
;                    (apply = c2-kid-diffs)) ;; this case will cause t-test to error
;             (if (= (first c1-kid-diffs) (first c2-kid-diffs))
;               1.0
;               0.0)
;             (:p-value (stats/t-test c1-kid-diffs :y c2-kid-diffs)))
;           0.01)))))))

;(defn reproductively-competent?
;  [g]
;  (let [num-kids 20
;        c1 (produce-child-genome-by-autoconstruction g g false)
;        c2 (produce-child-genome-by-autoconstruction g g false)]
;    (if (not (apply distinct? (map translate [g c1 c2])))
;      false
;      (let [c1-kids (for [_ (range num-kids)]
;                      (produce-child-genome-by-autoconstruction c1 g false))
;            c2-kids (for [_ (range num-kids)]
;                      (produce-child-genome-by-autoconstruction c2 g false))]
;        (if (not (apply distinct? (map translate (concat c1-kids c2-kids [g c1 c2]))))
;          false
;          (let [c1-kid-diffs (mapv #(expressed-difference c1 %) c1-kids)
;                c2-kid-diffs (mapv #(expressed-difference c2 %) c2-kids)]
;            (< (if (and (apply = c1-kid-diffs)
;                        (apply = c2-kid-diffs)) ;; this case will cause t-test to error
;                 1.0
;                 (:p-value (stats/t-test c1-kid-diffs :y c2-kid-diffs)))
;               0.01)))))))

(defn reproductively-competent?
  [g]
  (let [num-kids 20
        c1 (produce-child-genome-by-autoconstruction g g false)
        c2 (produce-child-genome-by-autoconstruction g g false)]
    (if (not (apply distinct? (map translate [g c1 c2])))
      false
      (let [c1-kids (for [_ (range num-kids)]
                      (produce-child-genome-by-autoconstruction c1 g false))
            c2-kids (for [_ (range num-kids)]
                      (produce-child-genome-by-autoconstruction c2 g false))]
        (if (not (apply distinct? (map translate (concat c1-kids c2-kids [g c1 c2]))))
          false
          (let [c1-kid-diffs (mapv #(expressed-difference c1 %) c1-kids)
                c2-kid-diffs (mapv #(expressed-difference c2 %) c2-kids)]
            (if (or (apply = c1-kid-diffs)
                    (apply = c2-kid-diffs))
              false
              (or (< (:p-value (stats/t-test c1-kid-diffs :y c2-kid-diffs))
                     0.01)
                  (< (stats/f-test c1-kid-diffs c2-kid-diffs)
                     0.01)))))))))

(defn autoconstruction
  "Returns a genome for child produced by autoconstruction by executing parent1 with parent1,
and parent2 on top of the genome stack. EXPERIMENTAL AND SUBJECT TO CHANGE."
  [parent1 parent2 {:keys [maintain-ancestors atom-generators max-points-in-initial-program error-function] 
                    :as argmap}]
  (let [parent1-genome (:genome parent1)
        parent2-genome (:genome parent2)
        child-genome  (produce-child-genome-by-autoconstruction parent1-genome parent2-genome false)
        competent (reproductively-competent? child-genome)
        new-genome (if competent
                     child-genome
                     (random-plush-genome max-points-in-initial-program atom-generators argmap))]
    (assoc (make-individual :genome new-genome
                            :history (:history parent1)
                            :ancestors (if maintain-ancestors
                                         (cons (:genome parent1) (:ancestors parent1))
                                         (:ancestors parent1)))
           :random-replacement-for-reproductively-incompetent-genome 
           (if competent false true))))
