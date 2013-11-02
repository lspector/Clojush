(ns clojush.random
  (:require [clj-random.core :as random])
  (:use [clojush.globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random code generator

#_(def ^:dynamic *thread-local-random-generator* (new java.util.Random))

(def ^:dynamic *thread-local-random-generator* (random/make-mersennetwister-rng))

#_(defn lrand-int
  "Return a random integer, using the thread-local random generator, that is less than the
   provided n. Arguments greater than 2^31-1 are treated as if they were 2^31-1 (2147483647)."
  [n]
  (if (<= n 1)
    0
    (if (integer? n)
      (. *thread-local-random-generator* (nextInt n))
      (. *thread-local-random-generator* (nextInt 2147483647))))) ;; biggest java.lang.Integer

(def lrand-int random/lrand-int)

#_(defn lrand
  "Return a random float between 0 and 1 usng the thread-local random generator."
  ([] (. *thread-local-random-generator* (nextFloat)))
  ([n] (* n (lrand))))

(def lrand random/lrand)

#_(defn lrand-nth
  "Return a random element of the collection."  
  [coll]
  (nth coll (. *thread-local-random-generator* (nextInt (count coll)))))

(def lrand-nth random/lrand-nth)

#_(defn lshuffle
  "Return a random permutation of coll (Adapted from clojure.core)"
  {:static true}
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al *thread-local-random-generator*)
    (clojure.lang.RT/vector (.toArray al))))

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
