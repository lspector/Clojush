(ns clojush.pushgp.genetic-operators
  (:use [clojush util random globals individual node-selection simplification interpreter]
        clojush.instructions.tag
        [clojure.math.numeric-tower])
  (:require [clojure.string :as string]))

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
  "For now just returns another random instruction. Should be changed to tweak
   the tag with Gaussian noise."
  [instr-map atom-generators epigenetic-markers close-parens-probabilities]
  (first (random-code 1 atom-generators epigenetic-markers close-parens-probabilities)))
  
(defn uniform-mutation
  "Uniformly mutates individual. For each token in program, there is
   uniform-mutation-rate probability of being mutated. If a token is to be
   mutated, it has a uniform-mutation-constant-tweak-rate probability of being
   mutated using a constant mutator (which varies depending on the type of the
   token), and otherwise is replaced with a random instruction."
  [ind {:keys [uniform-mutation-rate uniform-mutation-constant-tweak-rate
               mutation-float-gaussian-standard-deviation mutation-int-gaussian-standard-deviation
               mutation-string-char-change-rate maintain-ancestors
               atom-generators epigenetic-markers close-parens-probabilities]}]
  (let [string-tweak (fn [st]
                       (map (fn [c]
                              (if (< (lrand) mutation-string-char-change-rate)
                                (lrand-nth (concat ["\n" "\t"] (map (comp str char) (range 32 127))))
                                c))
                            st))
        instruction-mutator (fn [token]
                              (first (random-code 1 atom-generators epigenetic-markers close-parens-probabilities)))
        constant-mutator (fn [token]
                           (let [const (:instruction token)]
                             (if (tag-instruction? const)
                               (tag-gaussian-tweak token atom-generators epigenetic-markers close-parens-probabilities)
                               (assoc token
                                      :instruction
                                      (cond
                                        (float? const) (perturb-with-gaussian-noise mutation-float-gaussian-standard-deviation const)
                                        (integer? const) (round (perturb-with-gaussian-noise mutation-int-gaussian-standard-deviation const))
                                        (string? const) (string-tweak const)
                                        (or (= const true) (= const false)) (lrand-nth [true false])
                                        :else (:instruction (first (random-code 1 atom-generators epigenetic-markers close-parens-probabilities))))))))               
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
  [ind {:keys [uniform-close-mutation-rate close-increment-rate maintain-ancestors]}]
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
                                  (:ancestors ind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alternation

(defn remove-alternation-padding
  "Removes instances of 'alternation-padding from genome."
  [genome]
  (remove #{'alternation-padding} genome))

(defn alternation
  "Uniformly alternates between the two parents using a similar method to that
   used in ULTRA."
  [parent1 parent2 {:keys [alternation-rate alignment-deviation
                           max-points maintain-ancestors] :as argmap}]
  (let [g1 (:genome parent1)
        g2 (:genome parent2)
        s1 (vec (if (>= (count g1) (count g2))
                  g1
                  (concat g1 (repeat (- (count g2) (count g1)) 'alternation-padding))))
        s2 (vec (if (>= (count g2) (count g1))
                  g2
                  (concat g2 (repeat (- (count g1) (count g2)) 'alternation-padding))))
        new-genome (remove-alternation-padding
                     (loop [i 0
                            use-s1 (lrand-nth [true false])
                            result-genome []]
                       (if (or (>= i (count (if use-s1 s1 s2))) ;; finished current program
                               (> (count result-genome) (* 2 max-points))) ;; runaway growth
                         (apply list result-genome);; Return
                         (if (< (lrand) alternation-rate)
                           (recur (max 0 (+' i (Math/round (*' alignment-deviation (gaussian-noise-factor)))))
                                  (not use-s1)
                                  result-genome)
                           (recur (inc i)
                                  use-s1
                                  (conj result-genome (nth (if use-s1 s1 s2) i)))))))]
    (make-individual :genome new-genome
                     :history (:history parent1)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome parent1) (:ancestors parent1))
                                  (:ancestors parent1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genetic operators (NOT UPDATE FOR PLUSH BELOW HERE)

(defn delete-mutate
  "Returns the individual with between 1 and 4 points deleted. The points can be
   single instructions or parenthetical pairs. The number of points is based
   roughly on a binomial distribution with n=4 and p=0.25, moved up one so that
   0 is never chosen. This results in the following probabilities for numbers
   of deletions:
   p(1) = 0.32
   p(2) = 0.42
   p(3) = 0.21
   p(4) = 0.05"
  [ind {:keys [maintain-ancestors]}]
  (let [new-program (loop [prog (:program ind)
                           how-many (let [prob (lrand)]
                                      (cond
                                        (< prob 0.32) 1
                                        (< prob 0.74) 2
                                        (< prob 0.95) 3
                                        true 4))]
                      (if (zero? how-many)
                        prog
                        (recur (if (< (lrand) 0.1)
                                 (remove-paren-pair prog)
                                 (remove-code-at-point prog (lrand-int (count-points prog))))
                               (dec how-many))))]
    (make-individual :program new-program :history (:history ind)
                     :ancestors (if maintain-ancestors
                                  (cons (:program ind) (:ancestors ind))
                                  (:ancestors ind)))))

(defn tagging-mutate
  "Returns a version of the given individual with a piece of code replaced by a tag
   reference, and with an expression that tags the replaced code with the same tag added
   to the beginning of the individual's program."
  [ind tag-limit {:keys [max-points maintain-ancestors]
                  :as argmap}]
  (let [old-program (:program ind)
        index-to-tag (select-node-index old-program argmap)
        tag (rand-int tag-limit)
        tagging-instruction (symbol (str "tag_exec_" (str tag)))
        tag-ref-instruction (symbol (str "tagged_" (str tag))) 
        new-program (list (list tagging-instruction
                                (code-at-point old-program index-to-tag))
                          (insert-code-at-point old-program
                                                index-to-tag
                                                tag-ref-instruction))]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))
