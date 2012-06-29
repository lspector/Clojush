(ns clojush.experimental.scaled-errors
  (:require [clojure.math.numeric-tower :as math]))

(defn scaled-errors
  "A utility function for use in error functions, to implement error-scaling as described
   by Maarten Keijzer in Scaled Symbolic Regression, in Genetic Programming and Evolvable
   Machines 5(3), pp. 259-269, September 2004. This returns a sequence of scaled errors given
   a sequence of outputs, a sequence of targets, and a penalty. If there are any non-numeric
   items in the outputs, or if all of the outputs are the same, then all of the scaled errors
   will be equal to the penalty -- note that this means that you cannot use this method
   to solve a problem for which all targets are the same. An optional fourth argument,
   if true, causes the scaling slope and intercept to be printed; this is necessary for
   unscaling outputs of an evolved solution -- see examples/scaled_sextic.clj for an
   example."
  ([outputs targets penalty]
    (scaled-errors outputs targets penalty false))
  ([outputs targets penalty print-slope-and-intercept]
    (if (or (some #(not (number? %)) outputs) (apply = outputs))
      (doall (repeat (count outputs) penalty))
      (let [average-output (/ (reduce +' outputs) (count outputs))
            average-target (/ (reduce +' targets) (count targets))
            slope (/ 
                    (reduce +' 
                            (map (fn [target output]
                                   (*' (-' target average-target) 
                                       (-' output average-output)))
                                 targets
                                 outputs))
                    (reduce +'
                            (map (fn [output] (math/expt (-' output average-output) 2))
                                 outputs)))
            intercept (-' average-target  (*' slope average-output))]
        (when print-slope-and-intercept 
          (println "slope " slope ", intercept " intercept))
        (doall (map (fn [target output]
                      (math/expt (float (-' target (+' intercept (*' slope output)))) 2))
                    targets
                    outputs))))))
