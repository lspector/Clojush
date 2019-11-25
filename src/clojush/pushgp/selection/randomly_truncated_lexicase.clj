(ns clojush.pushgp.selection.randomly-truncated-lexicase
  (:use [clojush random]))

(defn randomly-truncated-lexicase-selection
  "Returns an individual that does the best on a random subset of the 
  fitness cases when considered one at a time in random order. The size
  of the subset considered is chosen with uniform probability from the
  range from 1 to the full number of available cases."
  [pop argmap]
  (let [num-cases (count (:errors (first pop)))]
    (loop [survivors pop
           cases (drop (if (< (lrand) (:randomly-truncated-lexicase-probability argmap))
                         (lrand-int (inc num-cases))
                         0)
                       (lshuffle (range num-cases)))]
      (if (or (empty? cases)
              (empty? (rest survivors))
              (< (lrand) (:lexicase-slippage argmap)))
        (lrand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))]
          (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                         survivors)
                 (rest cases)))))))

(defn truncated-lexicase-selection
  "Returns an individual selected by lexicase on a subset of the training
  cases, chosen at random for each selection. The subset size is fixed
  during evolution by the parameter truncated-lexicase-factor, which is
  a number in [0.0, 1.0] that determines what proportion of the cases to use."
  [pop {:keys [truncated-lexicase-factor] :as argmap}]
  (let [num-cases (count (:errors (first pop)))
        num-cases-to-use (Math/round (* num-cases truncated-lexicase-factor))]
    (loop [survivors pop
           cases (take num-cases-to-use
                       (lshuffle (range num-cases)))]
      (if (or (empty? cases)
              (empty? (rest survivors))
              (< (lrand) (:lexicase-slippage argmap)))
        (lrand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))]
          (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                         survivors)
                 (rest cases)))))))
