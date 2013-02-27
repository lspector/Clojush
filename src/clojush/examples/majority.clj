;; majority.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Kyle Harrington, kyleh@cs.brandeis.edu, 2011.

(ns clojush.examples.majority
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; The "majority" problem: have fewer negative complements than respective positive integers
;;

(defonce global-problem-size (atom 16))

(defn majority-fitness
  "Returns a fitness function for the lid problem for specified
depth and number of nodes."
  [program]
  (let [f (frequencies (filter number? (flatten program)))
        dfs (map #(if (or (and (contains? f %)
			       (not (contains? f (- %))))
			  (and (contains? f %)
			       (contains? f (- %))
			       (>= (- (f %) (f (- %))) 0)))
                    0 1) (range 1 (inc @global-problem-size)))]
    dfs))

(defn make-majority-instructions
  "Make the majority instructions for a given problem size."
  [problem-size]
  (list (fn [] (inc (rand-int problem-size)))
        (fn [] (- (inc (rand-int problem-size))))))

(defn majority-pushgp
  "Run Order with pushgp."
  [args]
  (let [size (or (:size args) 16)
        atom-generators (make-majority-instructions size)]
    (reset! global-problem-size size)
    (println "problem-size =" size)
    (def argmap
      {:max-points (* 10 size)
       :max-points-in-initial-program (* 10 size)
       :error-function majority-fitness
       :atom-generators atom-generators})))
  
(majority-pushgp {})