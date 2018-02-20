(ns clojush.pushgp.selection.random-toggle-lexicase
  (:use [clojush random]))

(defn random-toggle-lexicase-selection
  "Returns an individual that has the best error for each test case for which the toggle
  is 'on' when the cases when considered one at a time in random order. The toggle is 
  set randomly for each case as it is considered, according to the probability provided
  as the value of the random-toggle-lexicase-probability parameter."
  [pop argmap]
  (loop [survivors pop
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [toggle (<= (lrand) (:random-toggle-lexicase-probability argmap))]
        (if toggle
          (let [threshold (apply min (map #(nth % (first cases))
                                          (map :errors survivors)))]
            (recur (filter #(<= (nth (:errors %) (first cases)) threshold)
                           survivors)
                   (rest cases)))
          (recur survivors
                 (rest cases)))))))
          
