(ns clojush.pushgp.parent-selection
  (:use [clojush.random]
        [clojush.globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexicase selection

(defn retain-one-individual-per-error-vector
  "Retains one random individual to represent each error vector."
  [pop]
  (map lrand-nth (vals (group-by #(:errors %) pop))))
  
(defn lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
time in random order."
  [pop]
  (loop [survivors (retain-one-individual-per-error-vector pop)
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection

(defn select
  "Returns a selected parent."
  [pop tournament-size radius location]
  (if @global-use-lexicase-selection
    (lexicase-selection pop)
    (let [tournament-set 
          (doall
            (for [_ (range tournament-size)]
              (nth pop
                   (if (zero? radius)
                     (lrand-int (count pop))
                     (mod (+ location (- (lrand-int (+ 1 (* radius 2))) radius))
                          (count pop))))))
          err-fn (cond
                   @global-use-historically-assessed-hardness :hah-error
                   @global-use-rmse :rms-error
                   true :total-error)]
      (reduce (fn [i1 i2] (if (< (err-fn i1) (err-fn i2)) i1 i2))
              tournament-set))))
