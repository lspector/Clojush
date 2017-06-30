(ns clojush.experimental.decimation
  (:use [clojush.random]))

(defn decimate
  "Returns the subset of the provided population remaining after sufficiently many
   elimination tournaments to reach the provided target-size."
  [population target-size tournament-size]
  (let [popsize (count population)]
    (if (<= popsize target-size)
      population
      (recur (let [tournament-index-set 
                   (let [first-location (lrand-int popsize)]
                     (cons first-location
                           (doall
                             (for [_ (range (dec tournament-size))]
                               (lrand-int popsize)))))
                   victim-index
                   (reduce (fn [i1 i2] 
                             (if (> (:total-error (nth population i1))
                                    (:total-error (nth population i2)))
                               i1 
                               i2))
                           tournament-index-set)]
               (vec (concat (subvec population 0 victim-index)
                            (subvec population (inc victim-index)))))
             target-size tournament-size))))

