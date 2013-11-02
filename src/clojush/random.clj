(ns clojush.random
  (:require [clj-random.core :as random])
  (:use [clojush.globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random code generator

(def ^:dynamic *thread-local-random-generator* (random/make-mersennetwister-rng))

(def lrand-int random/lrand-int)

(def lrand random/lrand)

(def lrand-nth random/lrand-nth)

(def lshuffle random/lshuffle)

(defn decompose
  "Returns a list of at most max-parts numbers that sum to number.
   The order of the numbers is not random (you may want to shuffle it)."
  [number max-parts]
  (if (or (<= max-parts 1) (<= number 1))
    (list number)
    (let [this-part (if @global-use-bushy-code 
                      (dec number)
                      (inc (lrand-int (dec number))))]
      (cons this-part (decompose (-' number this-part)
                                 (dec max-parts))))))

(defn random-code-with-size
  "Returns a random expression containing the given number of points."
  [points atom-generators]
  (if (< points 2)
    (let [element (lrand-nth atom-generators)]
      (if (fn? element)
        (element)
        element))
    (let [elements-this-level 
          (lshuffle (decompose (dec points) (dec points)))]
      (doall (map (fn [size] (random-code-with-size size atom-generators))
                  elements-this-level)))))

(defn random-code 
  "Returns a random expression with size limited by max-points."
  [max-points atom-generators]
  (random-code-with-size (inc (lrand-int max-points)) atom-generators))
