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
  [argmap]
  (let [size (or (:size argmap) 16)	
        atom-generators (make-majority-instructions size)
	args (-> argmap
		 (assoc :max-points (* 10 size))
		 (assoc :error-function majority-fitness)
		 (assoc :atom-generators atom-generators))]
    (println "problem-size =" size)
    (reset! global-problem-size size)
    (pushgp-map args)))
;  (System/exit 0))

(majority-pushgp {})