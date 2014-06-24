(ns clojush.random
  (:require [clj-random.core :as random])
  (:use [clojush.globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random functions

(def ^:dynamic *thread-local-random-generator* (random/make-mersennetwister-rng))

(def lrand-int random/lrand-int)

(def lrand random/lrand)

(def lrand-nth random/lrand-nth)

(def lshuffle random/lshuffle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random code generator

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
  [atom-generators epigenetic-markers close-parens-probabilities]
  (let [markers (conj epigenetic-markers :instruction)]
    (zipmap markers
            (map (fn [marker]
                   (case marker
                     :instruction (let [element (lrand-nth atom-generators)]
                                    (if (fn? element)
                                      (element)
                                      element))
                     :close-parens (random-closes close-parens-probabilities)
                   ))
                 markers))))

(defn random-code-with-size
  "Returns a random Plush expression containing the given number of points."
  [points atom-generators epigenetic-markers close-parens-probabilities]
  (repeatedly points
              (partial random-plush-instruction-map
                       atom-generators
                       epigenetic-markers
                       close-parens-probabilities)))

(defn random-code 
  "Returns a random expression with size limited by max-points."
  [max-points atom-generators epigenetic-markers close-parens-probabilities]
    (random-code-with-size (inc (lrand-int max-points))
                           atom-generators
                           epigenetic-markers
                           close-parens-probabilities))
