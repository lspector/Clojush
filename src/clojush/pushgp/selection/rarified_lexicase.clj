(ns clojush.pushgp.selection.rarified-lexicase
  (:use [clojush random]))

#_(defn rarified-lexicase-selection
  "Returns an individual that does the best/rarest on the fitness cases when considered one at a
  time in random order."
  [pop argmap]
  (loop [survivors pop
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [criterion (if (< (lrand) 1/2) :best :rarest)
            errs (map #(nth % (first cases))
                      (map :errors survivors))
            efreqs (frequencies errs)
            min-for-case (case criterion
                           :best (apply min errs)
                           :rarest (apply min (vals efreqs)))]
        (recur (filter #(case criterion
                          :best (= (nth (:errors %) (first cases))
                                   min-for-case)
                          :rarest (= (get efreqs (nth (:errors %) (first cases)))
                                     min-for-case))
                       survivors)
               (rest cases))))))

#_(defn rarified-lexicase-selection
  "Returns an individual that does the best/rarest on the fitness cases when considered one at a
  time in random order."
  [pop argmap]
  (let [criterion (if (< (lrand) 1/2) :best :rarest)]
    (loop [survivors pop
           cases (lshuffle (range (count (:errors (first pop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (lrand-nth survivors)
        (let [errs (map #(nth % (first cases))
                        (map :errors survivors))
              efreqs (frequencies errs)
              min-for-case (case criterion
                             :best (apply min errs)
                             :rarest (apply min (vals efreqs)))]
          (recur (filter #(case criterion
                            :best (= (nth (:errors %) (first cases))
                                     min-for-case)
                            :rarest (= (get efreqs (nth (:errors %) (first cases)))
                                       min-for-case))
                         survivors)
                 (rest cases)))))))

(defn rarified-lexicase-selection
  "Returns an individual that does the best/rarest on the fitness cases when considered one at a
  time in random order."
  [pop argmap]
  (loop [survivors pop
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [errs (map #(nth % (first cases))
                      (map :errors survivors))
            efreqs (frequencies errs)
            min-err-for-case (apply min errs)
            min-freq-for-case (apply min (vals efreqs))]
        (recur (filter #(or (= (nth (:errors %) (first cases))
                               min-err-for-case)
                            (= (get efreqs (nth (:errors %) (first cases)))
                               min-freq-for-case))
                       survivors)
               (rest cases))))))
