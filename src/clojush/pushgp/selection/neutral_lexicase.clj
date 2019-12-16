(ns clojush.pushgp.selection.neutral-lexicase
  (:use [clojush random]
        [clojush.pushgp.selection lexicase]))


(defn neutral-lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order, with non-numbers counting equally with the best."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [errs (filter number?
                         (map #(nth % (first cases))
                              (map :errors survivors)))]
        (if (empty? errs)
          (recur survivors
                 (rest cases))
          (let [min-err-for-case (apply min errs)]
            (recur (filter #(let [e (nth (:errors %) (first cases))]
                              (or (not (number? e))
                                  (= e min-err-for-case)))
                           survivors)
                   (rest cases))))))))
