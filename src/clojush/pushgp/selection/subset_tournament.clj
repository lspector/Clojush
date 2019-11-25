(ns clojush.pushgp.selection.subset-tournament
  (:use [clojush random]))

(defn subset-tournament-selection
  "Returns an individual that does the best out of a tournament using only a subset
  of the error cases."
  [pop {:keys [tournament-size total-error-method] :as argmap}]
  (let [tournament-set (doall (for [_ (range tournament-size)]
                                (lrand-nth pop)))
        total-cases (count (:errors (first pop)))
        all-cases (range total-cases)
        cases (if (< (rand) 0.8)
                (take (inc (rand-int (dec total-cases)))
                      (shuffle all-cases))
                (if (< (rand 0.5))
                  [(rand-nth all-cases)]
                  all-cases))
        err-fn (fn [ind]
                 (apply + 
                        (for [c cases]
                            (nth (:errors ind) c))))]
    (apply min-key err-fn tournament-set)))
