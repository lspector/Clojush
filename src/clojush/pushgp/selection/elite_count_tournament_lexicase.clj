(ns clojush.pushgp.selection.elite-count-tournament-lexicase
  (:use [clojush random]))

(defn elite-diversity
  "Retuns the number of error vectors in pop with the best error for the given case."
  [pop c]
  (let [min-err-for-case (apply min (map #(nth % c)
                                         (map :errors pop)))]
    (count (distinct (map :errors (filter #(= (nth (:errors %) c) min-err-for-case)
                                          pop))))))

(defn elite-count-tournament-lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in an order determined by elite-count tournaments."
  [pop argmap]
  (loop [survivors pop
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [case-to-use (let [tournament-set (repeatedly (:elite-count-tournament-size argmap)
                                                         #(lrand-nth cases))]
                          (apply min-key (partial elite-diversity survivors) tournament-set))
            min-err-for-case (apply min (map #(nth % case-to-use)
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) case-to-use) min-err-for-case)
                       survivors)
               (remove #(= % case-to-use) cases))))))
