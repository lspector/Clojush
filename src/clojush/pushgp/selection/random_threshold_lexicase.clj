(ns clojush.pushgp.selection.random-threshold-lexicase
  (:use [clojush random]))

(defn random-threshold-lexicase-selection
  "Returns an individual that meets or improves upon an error threshold on the fitness 
  cases when considered one at a time in random order. The threshold is chosen randomly
  from those present in the population."
  [pop argmap]
  (loop [survivors pop
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [threshold (if (<= (lrand) (:random-threshold-lexicase-probability argmap))
                        (lrand-nth (distinct (map #(nth % (first cases))
                                                  (map :errors survivors))))
                        (apply min (map #(nth % (first cases))
                                        (map :errors survivors))))]
        (recur (filter #(<= (nth (:errors %) (first cases)) threshold)
                       survivors)
               (rest cases))))))

