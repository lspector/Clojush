
(ns clojush.random
  (:use [clojush globals translate])
  (:require [clj-random.core :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random functions

(def ^:dynamic *thread-local-random-generator* (random/make-mersennetwister-rng))

(def lrand-int random/lrand-int)

(def lrand random/lrand)

(def lrand-nth random/lrand-nth)

(def lshuffle random/lshuffle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random plush genome generator

(defn random-closes
  "Returns a random number of closes based on close-parens-probabilities, which
   defaults to [0.772 0.206 0.021 0.001]. This is roughly equivalent to each selection
   coming from  a binomial distribution with n=4 and p=1/16.
      (see http://www.wolframalpha.com/input/?i=binomial+distribution+4+0.0625)
   This results in the following probabilities:
     p(0) = 0.772
     p(1) = 0.206
     p(2) = 0.021
     p(3) = 0.001"
  [close-parens-probabilities]
  (let [prob (lrand)]
    (loop [parens 0
           probabilities (concat (reductions + close-parens-probabilities)
                                 '(1.0))]
      (if (<= prob (first probabilities))
        parens
        (recur (inc parens)
               (rest probabilities))))))

(defn random-plush-instruction-map
  "Returns a random instruction map given the atom-generators and the required
   epigenetic-markers."
  ([atom-generators]
   (random-plush-instruction-map atom-generators {}))
  ([atom-generators  {:keys [epigenetic-markers
                             close-parens-probabilities
                             silent-instruction-probability
                             track-instruction-maps]
                      :or {epigenetic-markers []
                           close-parens-probabilities [0.772 0.206 0.021 0.001]
                           silent-instruction-probability 0}}]
   (let [markers (cond->
                     (conj epigenetic-markers :instruction)
                   track-instruction-maps (conj :uuid :random-insertion))]
      (zipmap markers
              (map (fn [marker]
                     (case marker
                       :instruction (let [element (lrand-nth atom-generators)]
                                      (if (fn? element)
                                        (let [fn-element (element)]
                                          (if (fn? fn-element)
                                            (fn-element)
                                            fn-element))
                                        element))
                       :close (random-closes close-parens-probabilities)
                       :silent (if (< (lrand) silent-instruction-probability)
                                 true
                                 false)
                       :random-insertion true
                       :uuid (java.util.UUID/randomUUID)
                       ))
                   markers)))))

(defn random-plush-genome-with-size
  "Returns a random Plush genome containing the given number of points."
  [genome-size atom-generators argmap]
  (vec (repeatedly genome-size
                   #(random-plush-instruction-map
                      atom-generators
                      argmap))))

(defn random-plush-genome
  "Returns a random Plush genome with size limited by max-genome-size."
  ([max-genome-size atom-generators]
    (random-plush-genome max-genome-size atom-generators {}))
  ([max-genome-size atom-generators argmap]
    (random-plush-genome-with-size (inc (lrand-int max-genome-size))
                                   atom-generators
                                   argmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random Push code generator

(defn random-push-code
  "Returns a random Push expression with size limited by max-points."
  ([max-points atom-generators]
    (random-push-code max-points atom-generators {:max-points @global-max-points}))
  ([max-points atom-generators argmap]
    (translate-plush-genome-to-push-program
      {:genome (random-plush-genome (max (int (/ max-points 4)) 1)
                                    atom-generators
                                    argmap)}
      argmap)))

