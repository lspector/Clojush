;; order.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Kyle Harrington, kyleh@cs.brandeis.edu, 2011.

(ns clojush.examples.order
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; The "order" problem: put positive numbers in front of their negative compliments
;;

(defonce global-problem-size (atom 16))

(defn order-fitness
  "Returns a fitness function for the order problem for specified
depth and number of nodes."
  [program]
  (loop [f (distinct (filter number? (flatten program)))
         P '()]
    (if (empty? f)
      (let [sp (set P)]
        (map #(if (contains? sp %) 0 1) (range 1 (inc @global-problem-size))))
      (if (zero? (count (filter #(= (abs (first f))
                                    (abs %)) P)))
        (recur (rest f) (cons (first f) P))
        (recur (rest f) P)))))

(defn make-order-instructions
  "Make the order instructions for a given problem size."
  [problem-size]
  (list (fn [] (inc (rand-int problem-size)))
        (fn [] (- (inc (rand-int problem-size))))))

(defn order-pushgp
  "Run Order with pushgp."
  [args]
  (let [size (or (:size args) 16)	
        atom-generators (make-order-instructions size)]
    (reset! global-problem-size size)
    (println "problem-size =" size)
    (def argmap
      {:max-points (* 10 size)
      :max-points-in-initial-program (* 10 size)
      :error-function order-fitness
      :atom-generators atom-generators})))

;  (System/exit 0))

(order-pushgp {})
