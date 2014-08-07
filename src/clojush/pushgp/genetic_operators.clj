(ns clojush.pushgp.genetic-operators
  (:use [clojush util random individual globals]
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
        s1 g1 #_(vec (if (>= (count g1) (count g2))
                      g1
                      (concat g1 (repeat (- (count g2) (count g1)) 'alternation-padding))))
        s2 g2 #_(vec (if (>= (count g2) (count g1))
                      g2
                      (concat g2 (repeat (- (count g1) (count g2)) 'alternation-padding))))
        new-genome (remove-alternation-padding
                     (loop [i 0
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
                                  (conj result-genome (nth (if use-s1 s1 s2) i)))))))]
    (make-individual :genome new-genome
                     :history (:history parent1)
                     :ancestors (if maintain-ancestors
                                  (cons (:genome parent1) (:ancestors parent1))
                                  (:ancestors parent1)))))
