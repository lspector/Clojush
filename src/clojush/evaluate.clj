(ns clojush.evaluate
  (:use [clojush util pushstate random globals individual]
        clojush.pushgp.genetic-operators)
  (:require [clojure.math.numeric-tower :as math]
            [clj-random.core :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate the solution rates (only called from pushgp)

(defn calculate-hah-solution-rates
  [pop-agents {:keys [total-error-method error-threshold population-size]}]
  (when (= total-error-method :hah)
    (reset! solution-rates
            (let [error-seqs (map :errors (map deref pop-agents))
                  num-cases (count (first error-seqs))]
              (doall (for [i (range num-cases)]
                       (/ (count (filter #(<= % error-threshold)
                                         (map #(nth % i) error-seqs)))
                          population-size)))))
    (printf "\nSolution rates: ")
    (println (doall (map float @solution-rates)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate meta-errors

(defn evaluate-individual-meta-errors
  "Calculates one meta-error for each meta-error category provided. Each
  meta-error-category should either be a keyword for a built-in meta category
  or a function that takes an individual and an argmap and returns a meta error value.
  The built-in meta categories include:
  :size (minimize size of program)
  :compressibility (minimize ammount a program compresses compared to itself)
  :total-error (minimize total error)
  :unsolved-cases (maximize number of cases with zero error)
  :rand (a random floating-point value)
  :rand-bit (randomly 0 or 1)
  :age (minimize genealogical age of program)"
  [ind evaluated-population rand-gen
   {:keys [meta-error-categories error-threshold improvement-discount] :as argmap}]
  (random/with-rng 
    rand-gen
    (let [meta-error-fn 
          (fn [cat]
            (cond
              (fn? cat) (cat ind argmap)
              (= cat :size) (count (:genome ind))
              ;(= cat :compressibility) 555 ;;TMH fix later
              (= cat :total-error) (:total-error ind)
              (= cat :unsolved-cases) (count (filter #(> % error-threshold) 
                                                     (:errors ind)))
              (= cat :rand) (lrand)
              (= cat :rand-bit) (lrand-nth [0 1])
              (= cat :age) (:age ind)
              (= cat :novelty) :novelty ; Keyword will be replaced later,
              ;                         ; needs entire population to compute novelty
              ;
              (= cat :gens-since-total-error-change)
              (if (not (:print-history argmap))
                (throw 
                  (Exception. 
                    ":print-history must be true for :gens-since-total-error-change"))
                (let [hist (mapv (partial reduce +) (:history ind))]
                  (if (or (empty? hist)
                          (apply = hist))
                    1000000
                    (count (take-while #(= % (first hist)) (rest hist))))))
              ;
              (= cat :gens-since-total-error-improvement)
              (if (not (:print-history argmap))
                (throw 
                  (Exception. 
                    ":print-history must be true for :gens-since-total-error-improvement"))
                (let [diffs (mapv (fn [[a b]] (- a b)) 
                                  (partition 2 1 (mapv (partial reduce +) 
                                                       (:history ind))))]
                  (if (or (empty? diffs)
                          (not (some neg? diffs)))
                    1000000
                    (count (take-while #(>= % 0) diffs)))))
              ;
              (= cat :total-error-improvement-ratio)
              (if (not (:print-history argmap))
                (throw 
                  (Exception. 
                    ":print-history must be true for :total-error-improvement-ratio"))
                (let [diffs (mapv (fn [[a b]] (- a b)) 
                                  (partition 2 1 (mapv (partial reduce +) 
                                                       (:history ind))))]
                  (if (empty? diffs)
                    1000000
                    (- 1 (/ (count (filter neg? diffs))
                            (count diffs))))))
              ;
              (= cat :total-error-new-best-ratio)
              (if (not (:print-history argmap))
                (throw 
                  (Exception. 
                    ":print-history must be true for :total-error-new-best-ratio"))
                (let [hist (mapv (partial reduce +) (:history ind))]
                  (if (empty? (rest hist))
                    1000000
                    (loop [remaining hist
                           new-best-count 0]
                      (if (empty? (rest remaining))
                        (- 1 (/ new-best-count (dec (count hist))))
                        (recur (rest remaining)
                               (+ new-best-count
                                  (if (every? #(> % (first remaining))
                                              (rest remaining))
                                    1
                                    0))))))))
              ;
              (= cat :discounted-total-error-new-best-ratio)
              (if (not (:print-history argmap))
                (throw 
                  (Exception. 
                    ":print-history must be true for :discounted-total-error-new-best-ratio"))
                (let [hist (mapv (partial reduce +) (:history ind))]
                  (if (empty? (rest hist))
                    1000000
                    (loop [remaining hist
                           new-best-count 0
                           scale 1
                           max-total 0]
                      (if (empty? (rest remaining))
                        (- 1.0 (/ new-best-count max-total))
                        (recur (rest remaining)
                               (+ new-best-count
                                  (if (every? #(> % (first remaining))
                                              (rest remaining))
                                    (/ 1 scale)
                                    0))
                               (* 2.0 scale)
                               (+ max-total (/ 1 scale))))))))
              ;
              (= cat :discounted-total-error-improvement)
              (if (not (:print-history argmap))
                (throw 
                  (Exception. 
                    ":print-history must be true, :discounted-total-error-improvement-ratio"))
                (if (empty? (rest (:history ind)))
                  1000000
                  (let [diffs (mapv (fn [[a b]] (- a b))
                                    (partition 2 1 (mapv (partial reduce +) (:history ind))))
                        improvements (mapv #(if (neg? %) 1.0 0.0) 
                                           diffs)
                        persistence 0.5
                        weights (take (count diffs) 
                                      (iterate (partial * persistence) 0.5))
                        sum (reduce + (mapv * improvements weights))]
                    (if (<= sum 0)
                      1.0E100
                      (- 1.0 sum)))))
              ;
              (= cat :case-stagnation) ;; formerly :discounted-case-improvements
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :case-stagnation"))
                (if (empty? (rest (:history ind)))
                  (vec (repeat (count (:errors ind)) 1000000))
                  (vec (for [case-history (apply map list (:history ind))]
                         (if (zero? (first case-history))
                           -100000  ;; solved, improvement doesn't matter
                           (let [improvements (mapv (fn [[newer-error older-error]]
                                                      (if (< newer-error older-error)
                                                        1.0
                                                        (if true ;(= newer-error older-error) ;for now only improvement counts
                                                          0.0
                                                          -1.0)))
                                                    (partition 2 1 case-history))
                                 weights (take (count improvements)
                                               (iterate (partial * (- 1 improvement-discount)) 1))
                                 sum (reduce + (mapv * improvements weights))]
                             (- sum)))))))
              ;
              (= cat :case-gens-since-improvement)
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :case-gens-since-improvement"))
                (let [huge 1000000]
                  (if (empty? (rest (:history ind)))
                    (vec (repeat (count (:errors ind)) huge))
                    (vec (for [case-history (apply map list (:history ind))]
                           (if (zero? (first case-history))
                             (- huge)  ;; solved, improvement doesn't matter
                             (let [improved? (mapv (fn [[newer-error older-error]]
                                                     (< newer-error older-error))
                                                   (partition 2 1 case-history))
                                   gens (take-while not improved?)]
                               (if (= (count gens)
                                      (count improved?))
                                 huge
                                 (count gens)))))))))
              (= cat :case-gens-since-change)
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :case-gens-since-change"))
                (let [huge 1000000]
                  (if (empty? (rest (:history ind)))
                    (vec (repeat (count (:errors ind)) huge))
                    (vec (for [case-history (apply map list (:history ind))]
                           (if (zero? (first case-history))
                             (- huge)  ;; solved, improvement doesn't matter
                             (let [changed? (mapv (fn [[newer-error older-error]]
                                                    (not= newer-error older-error))
                                                  (partition 2 1 case-history))
                                   gens (take-while not changed?)]
                               (if (= (count gens)
                                      (count changed?))
                                 huge
                                 (count gens)))))))))
              ;
              (= cat :case-sibling-uniformity)
              (if (empty? (:parent-uuids ind))
                (vec (repeat (count (:errors ind)) 1))
                (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                                (first (:parent-uuids %)))
                                             (not (:is-random-replacement %))) ; new random sibs don't count
                                       evaluated-population)]
                  (vec (for [case-index (range (count (:errors ind)))]
                         (if (zero? (nth (:errors ind) case-index)) ;; solved
                           0 
                           (if (or (empty? siblings)
                                   (apply = (mapv #(nth (:errors %) case-index)
                                                  siblings)))
                             1
                             0))))))
              ;
              (= cat :case-family-uniformity)
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :case-family-uniformity"))
                (if (or (empty? (:parent-uuids ind))
                        (empty? (rest (:history ind))))
                  (vec (repeat (count (:errors ind)) 1))
                  (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                                  (first (:parent-uuids %)))
                                               (not (empty? (rest (:history %))))) ; new random sibs don't count
                                         evaluated-population)]
                    (vec (for [case-index (range (count (:errors ind)))]
                           (if (zero? (nth (:errors ind) case-index)) ;; solved
                             0 
                             (if 
                               (some (fn [sib]
                                       (or (zero? (-> (:history sib) ;; sib solved
                                                      (first)
                                                      (nth case-index)))
                                           (not= (-> (:history sib) ;; sib error different than mom's
                                                     (first)
                                                     (nth case-index))
                                                 (-> (:history sib)
                                                     (second)
                                                     (nth case-index)))))
                                     siblings)
                               0
                               1)))))))
              ;
              (= cat :case-family-certainty)
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :case-family-certainty"))
                (if (or (empty? (:parent-uuids ind))
                        (empty? (rest (:history ind))))
                  (vec (repeat (count (:errors ind)) 1))
                  (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                                  (first (:parent-uuids %)))
                                               (not (empty? (rest (:history %))))) ; new random sibs don't count
                                         evaluated-population)]
                    (vec (for [case-index (range (count (:errors ind)))]
                           (if (zero? (nth (:errors ind) case-index)) ;; solved
                             0 
                             (/ 1 (count 
                                    (distinct 
                                      (for [s siblings] 
                                        (nth (first (:history s)) 
                                             case-index)))))))))))
              ;
              (= cat :family-uniformity)
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :family-variation"))
                (if (or (empty? (:parent-uuids ind))
                        (empty? (rest (:history ind))))
                  1
                  (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                                  (first (:parent-uuids %)))
                                               (not (empty? (rest (:history %))))) ; new random sibs don't count
                                         evaluated-population)]
                    (if (some (fn [sib]
                                (not= (first (:history sib))
                                      (second (:history sib))))
                              siblings)
                      0
                      1))))
              ;
              (= cat :repeated-errors)
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (or (empty? (rest (:history ind)))
                        (some #{(first (:history ind))}
                              (set (rest (:history ind)))))
                    1
                    0))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (some #{(first (:history ind))}
                          (set (rest (:history ind))))
                    1
                    0))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (or (empty? (rest (:history ind)))
                        (some #{(first (:history ind))}
                              (set (take 3 (rest (:history ind))))))
                    1
                    0))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (empty? (rest (:history ind)))
                  1
                  (if (some #{(first (:history ind))}
                            (set (rest (:history ind))))
                    (/ 1 (inc (count (take-while #(not= % (first (:history ind)))
                                                 (rest (:history ind))))))
                    0)))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (empty? (rest (:history ind)))
                  (vec (repeat (count (:errors ind)) 1))
                  (vec (for [case-index (range (count (:errors ind)))]
                         (if (zero? (nth (:errors ind) case-index)) ;; solved
                           0 
                           (let [latest (nth (first (:history ind)) case-index)
                                 olders (map #(nth % case-index) (rest (:history ind)))]
                             (if (some #{latest} (set olders))
                               (/ 1 (inc (count (take-while #(not= % latest) olders))))
                               0)))))))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (empty? (rest (:history ind)))
                  (vec (repeat (count (:errors ind)) 2))
                  (vec (for [case-index (range (count (:errors ind)))]
                         (if (zero? (nth (:errors ind) case-index)) ;; solved
                           0 
                           (let [latest (nth (first (:history ind)) case-index)
                                 olders (map #(nth % case-index) (rest (:history ind)))]
                             (if (some #{latest} (set olders))
                               2
                               1)))))))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (empty? (rest (:history ind)))
                  (vec (repeat (count (:errors ind)) 2))
                  (vec (for [case-index (range (count (:errors ind)))]
                         (if (zero? (nth (:errors ind) case-index)) ;; solved
                           0 
                           (let [latest (nth (first (:history ind)) case-index)
                                 olders (map #(nth % case-index) (rest (:history ind)))]
                             (if (some #{latest} (set olders))
                               3
                               1)))))))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (empty? (rest (:history ind)))
                  1
                  (let [latest (first (:history ind))
                        olders (rest (:history ind))]
                    (if (some #{latest} (set olders))
                      2
                      0))))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (empty? (rest (:history ind)))
                  (vec (repeat (count (:errors ind)) 1))
                  (vec (for [case-index (range (count (:errors ind)))]
                         (let [latest (nth (first (:history ind)) case-index)
                               olders (map #(nth % case-index) (rest (:history ind)))]
                           (if (some #{latest} (set olders))
                             2
                             0))))))
              #_(if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (empty? (rest (:history ind)))
                  (vec (repeat (count (:errors ind)) 1))
                  (vec (for [case-index (range (count (:errors ind)))]
                         (let [latest (nth (first (:history ind)) case-index)
                               olders (map #(nth % case-index) (take 3 (rest (:history ind))))]
                           (if (some #{latest} (set olders))
                             2
                             0))))))
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :repeated-errors"))
                (if (empty? (rest (:history ind)))
                  (vec (repeat (count (:errors ind)) 1))
                  (vec (for [case-index (range (count (:errors ind)))]
                         (let [latest (nth (first (:history ind)) case-index)
                               olders (map #(nth % case-index) (rest (:history ind)))]
                           (if (some #{latest} (set olders))
                             2
                             0))))))
              ;
              (= cat :error-favoritism)
              (if (and (:parent1-errors ind) (:parent2-errors ind))
                (math/abs (- (sequence-similarity (:errors ind) (:parent1-errors ind))
                             (sequence-similarity (:errors ind) (:parent2-errors ind))))
                1.0)
              ;
              (= cat :case-error-neglect)
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (if (== e (min p1e p2e))
                         0
                         1)))
                (vec (repeat (count (:errors ind)) 1)))
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (if (and (not= p1e p2e)
                                (== e (min p1e p2e)))
                         0
                         1)))
                (vec (repeat (count (:errors ind)) 1)))
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (if (< e (max p1e p2e))
                         0
                         1)))
                (vec (repeat (count (:errors ind)) 1)))
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (< e (min p1e p2e)) 0
                         (= e (min p1e p2e)) 1
                         (< e (max p1e p2e)) 2
                         (= e (max p1e p2e)) 3
                         :else 4)))
                (vec (repeat (count (:errors ind)) 5)))
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (= e (min p1e p2e)) 2
                         (< e (max p1e p2e)) 3
                         (= e (max p1e p2e)) 4
                         :else 5)))
                (vec (repeat (count (:errors ind)) 6)))
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 6
                         (< e (min p1e p2e)) 0
                         (= e (min p1e p2e)) 1
                         (< e (max p1e p2e)) 2
                         (= e (max p1e p2e)) 3
                         :else 4)))
                (vec (repeat (count (:errors ind)) 5)))
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (< e (min p1e p2e)) 0
                         (= e (min p1e p2e)) 2
                         (< e (max p1e p2e)) 1
                         (= e (max p1e p2e)) 4
                         :else 3)))
                (vec (repeat (count (:errors ind)) 5)))
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (= e (min p1e p2e)) 3
                         (< e (max p1e p2e)) 2
                         (= e (max p1e p2e)) 5
                         :else 4)))
                (vec (repeat (count (:errors ind)) 6)))
              (if (and (:parent1-errors ind) (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (and (not= p1e p2e) (= e (min p1e p2e))) 2
                         (< e (max p1e p2e)) 3
                         (= e (max p1e p2e)) 4
                         :else 5)))
                (vec (repeat (count (:errors ind)) 6)))
              ;
              (= cat :devolution)
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind)
                       (not= (:errors ind) (:parent1-errors ind))
                       (not= (:errors ind) (:parent2-errors ind)))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (and (not= p1e p2e) (= e (min p1e p2e))) 2
                         (< e (max p1e p2e)) 3
                         (= e (max p1e p2e)) 4
                         :else 5)))
                (vec (repeat (count (:errors ind)) 6)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (if (or (= (:errors ind) (:parent1-errors ind))
                        (= (:errors ind) (:parent2-errors ind)))
                  (vec (repeat (count (:errors ind)) 7))
                  (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                               (:errors ind)
                                               (:parent1-errors ind)
                                               (:parent2-errors ind))]
                         (cond 
                           (zero? e) 0
                           (< e (min p1e p2e)) 1
                           (and (not= p1e p2e) (= e (min p1e p2e))) 2
                           (< e (max p1e p2e)) 3
                           (= e (max p1e p2e)) 4
                           :else 5))))
                (vec (repeat (count (:errors ind)) 6)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (if (or (= (:errors ind) (:parent1-errors ind))
                        (= (:errors ind) (:parent2-errors ind)))
                  (vec (repeat (count (:errors ind)) 7))
                  (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                               (:errors ind)
                                               (:parent1-errors ind)
                                               (:parent2-errors ind))]
                         (cond 
                           ;(zero? e) 0
                           (< e (min p1e p2e)) 1
                           (and (not= p1e p2e) (= e (min p1e p2e))) 2
                           (< e (max p1e p2e)) 3
                           (= e (max p1e p2e)) 4
                           :else 5))))
                (vec (repeat (count (:errors ind)) 6)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (if (or (= (:errors ind) (:parent1-errors ind))
                        (= (:errors ind) (:parent2-errors ind)))
                  (vec (repeat (count (:errors ind)) 7))
                  (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                               (:errors ind)
                                               (:parent1-errors ind)
                                               (:parent2-errors ind))]
                         (cond 
                           (zero? e) 1
                           (< e (min p1e p2e)) 1
                           (and (not= p1e p2e) (= e (min p1e p2e))) 2
                           (< e (max p1e p2e)) 3
                           (= e (max p1e p2e)) 4
                           :else 5))))
                (vec (repeat (count (:errors ind)) 6)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (if (or (= (:errors ind) (:parent1-errors ind))
                        (= (:errors ind) (:parent2-errors ind)))
                  (vec (repeat (count (:errors ind)) 5))
                  (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                               (:errors ind)
                                               (:parent1-errors ind)
                                               (:parent2-errors ind))]
                         (cond 
                           (or (zero? e) (< e (min p1e p2e))) 0
                           (some #{e} [p1e p2e]) 3
                           (< e (max p1e p2e)) 1
                           :else 2))))
                (vec (repeat (count (:errors ind)) 4)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (and (not= p1e p2e) (= e (min p1e p2e))) 2
                         (< e (max p1e p2e)) 3
                         (= e (max p1e p2e)) 4
                         :else 5)))
                (vec (repeat (count (:errors ind)) 6)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 1
                         (< e (min p1e p2e)) 1
                         (and (not= p1e p2e) (= e (min p1e p2e))) 2
                         (< e (max p1e p2e)) 3
                         (= e (max p1e p2e)) 4
                         :else 5)))
                (vec (repeat (count (:errors ind)) 6)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (or (zero? e) (< e (min p1e p2e))) 0
                         (some #{e} [p1e p2e]) 3
                         (< e (max p1e p2e)) 1
                         :else 2)))
                (vec (repeat (count (:errors ind)) 3)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (some #{e} [p1e p2e]) 4
                         (< e (max p1e p2e)) 2
                         :else 3)))
                (vec (repeat (count (:errors ind)) 4)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (some #{e} [p1e p2e]) 3
                         (< e (max p1e p2e)) 2
                         :else 4)))
                (vec (repeat (count (:errors ind)) 4)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 1
                         (< e (min p1e p2e)) 1
                         (some #{e} [p1e p2e]) 3
                         (< e (max p1e p2e)) 2
                         :else 4)))
                (vec (repeat (count (:errors ind)) 4)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (< e (max p1e p2e)) 2
                         :else 3)))
                (vec (repeat (count (:errors ind)) 3)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (< e (min p1e p2e)) 0
                         (< e (max p1e p2e)) 1
                         :else 2)))
                (vec (repeat (count (:errors ind)) 2)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (= e (min p1e p2e)) 2
                         (< e (max p1e p2e)) 3
                         (= e (max p1e p2e)) 4
                         :else 5)))
                (vec (repeat (count (:errors ind)) 5)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 1
                         (< e (min p1e p2e)) 1
                         (= e (min p1e p2e)) 2
                         (< e (max p1e p2e)) 3
                         (= e (max p1e p2e)) 4
                         :else 5)))
                (vec (repeat (count (:errors ind)) 5)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (= e (min p1e p2e)) 2
                         (< e (max p1e p2e)) 3
                         (= e (max p1e p2e)) 5
                         :else 4)))
                (vec (repeat (count (:errors ind)) 5)))
              #_(if (and (:parent1-errors ind)
                       (:parent2-errors ind))
                (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                             (:errors ind)
                                             (:parent1-errors ind)
                                             (:parent2-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e (min p1e p2e)) 1
                         (= e (min p1e p2e)) 2
                         (< e (max p1e p2e)) 2
                         (= e (max p1e p2e)) 2
                         :else 3)))
                (vec (repeat (count (:errors ind)) 3)))
              #_(if (:parent1-errors ind)
                (vec (for [[e p1e] (mapv #(vector %1 %2)
                                         (:errors ind)
                                         (:parent1-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e p1e) 1
                         (= e p1e) 2
                         :else 3)))
                (vec (repeat (count (:errors ind)) 3)))
              #_(if (:parent1-errors ind)
                (vec (for [[e p1e] (mapv #(vector %1 %2)
                                         (:errors ind)
                                         (:parent1-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e p1e) 1
                         (= e p1e) 3
                         :else 2)))
                (vec (repeat (count (:errors ind)) 2)))
              (if (:parent1-errors ind)
                (vec (for [[e p1e] (mapv #(vector %1 %2)
                                         (:errors ind)
                                         (:parent1-errors ind))]
                       (cond 
                         (zero? e) 0
                         (< e p1e) 1
                         (= e p1e) 2
                         :else 2)))
                (vec (repeat (count (:errors ind)) 2)))
              ;
              (= cat :error-neglect)
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (reduce + (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                                  (:errors ind)
                                                  (:parent1-errors ind)
                                                  (:parent2-errors ind))]
                            (if (and (not= p1e p2e)
                                     (== e (min p1e p2e)))
                              0
                              1)))
                (count (:errors ind)))
              #_(if (and (:parent1-errors ind) (:parent2-errors ind))
                (reduce + (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                                  (:errors ind)
                                                  (:parent1-errors ind)
                                                  (:parent2-errors ind))]
                            (if (< e (max p1e p2e))
                              0
                              1)))
                (count (:errors ind)))
              (if (and (:parent1-errors ind) (:parent2-errors ind))
                (reduce + (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                                  (:errors ind)
                                                  (:parent1-errors ind)
                                                  (:parent2-errors ind))]
                            (if (<= e (min p1e p2e))
                              0
                              1)))
                (count (:errors ind)))
              ;
              (= cat :case-family-variation)
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :case-family-variation"))
                (if (or (empty? (:parent-uuids ind))
                        (empty? (rest (:history ind))))
                  (vec (repeat (count (:errors ind)) 1))
                  (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                                  (first (:parent-uuids %)))
                                               (not (empty? (rest (:history %))))) ; new random sibs don't count
                                         evaluated-population)]
                    (vec (for [case-index (range (count (:errors ind)))]
                           (if (zero? (nth (:errors ind) case-index)) ;; solved
                             0 
                             (if 
                               (some (fn [sib] ;; sibling solved
                                       (zero? (-> (:history sib)
                                                  (first)
                                                  (nth case-index))))
                                     siblings)
                               0
                               (if (some (fn [sib] ;; sib error different than mom's
                                           (not= (-> (:history sib)
                                                     (first)
                                                     (nth case-index))
                                                 (-> (:history sib)
                                                     (second)
                                                     (nth case-index))))
                                         siblings)
                                 1
                                 0))))))))
              ;
              (= cat :washout-mother)
              (if (not (:print-history argmap))
                (throw
                  (Exception.
                    ":print-history must be true for :washout-mother"))
                (if (or (empty? (:parent-uuids ind))
                        (empty? (rest (:history ind))))
                  (vec (repeat (count (:errors ind)) 1))
                  (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                                  (first (:parent-uuids %)))
                                               (not (empty? (rest (:history %))))) ; new random sibs don't count
                                         evaluated-population)]
                    (vec (for [case-index (range (count (:errors ind)))]
                           (if (zero? (nth (:errors ind) case-index)) ;; solved
                             0 
                             (if 
                               (some (fn [sib]
                                       (or (zero? (-> (:history sib)
                                                      (first)
                                                      (nth case-index)))
                                           (< (-> (:history sib)
                                                  (first)
                                                  (nth case-index))
                                              (-> (:history sib)
                                                  (second)
                                                  (nth case-index)))))
                                     siblings)
                               0
                               1)))))))
              ;
              (= cat :clone-distance)
              (/ 1 (let [limit 100]
                     (loop [step 1
                            genomes (list (:genome ind))]
                       (if (>= step limit)
                         step
                         (let [child (produce-child-genome-by-autoconstruction 
                                       (first genomes) 
                                       (first genomes) 
                                       argmap)]
                           (if (some #{child} (set genomes))
                             step
                             (recur (inc step) (cons child genomes))))))))
              ;
              (= cat :reproductive-infidelity)
              (let [g (:genome ind)]
                (- 1.0
                   (sequence-similarity
                     g
                     (produce-child-genome-by-autoconstruction g g argmap))))
              ;
              (= cat :reproductive-fidelity)
              (let [g (:genome ind)]
                (sequence-similarity
                  g
                  (produce-child-genome-by-autoconstruction g g argmap)))
              ;
              (= cat :reproductive-inconsistency)
              (let [g (:genome ind)]
                (- 1.0
                   (sequence-similarity
                     (produce-child-genome-by-autoconstruction g g argmap)
                     (produce-child-genome-by-autoconstruction g g argmap))))
              ;
              (= cat :reproductive-consistency)
              (let [g (:genome ind)]
                (sequence-similarity
                  (produce-child-genome-by-autoconstruction g g argmap)
                  (produce-child-genome-by-autoconstruction g g argmap)))
              ;
              (= cat :similarity-to-most-similar-parent)
              (if (and (:parent1-genome ind) (:parent2-genome ind))
                (max (sequence-similarity (:genome ind) (:parent1-genome ind))
                     (sequence-similarity (:genome ind) (:parent2-genome ind)))
                1.0)
              ;
              (= cat :difference-from-most-similar-parent)
              (- 1.0 (if (and (:parent1-genome ind) (:parent2-genome ind))
                       (max (sequence-similarity (:genome ind) (:parent1-genome ind))
                            (sequence-similarity (:genome ind) (:parent2-genome ind)))
                       0.0))
              ;
              (= cat :difference-from-least-similar-parent)
              (- 1.0 (if (and (:parent1-genome ind) (:parent2-genome ind))
                       (min (sequence-similarity (:genome ind) (:parent1-genome ind))
                            (sequence-similarity (:genome ind) (:parent2-genome ind)))
                       0.0))
              ;
              (= cat :difference-from-mate)
              (- 1.0 (if (:parent2-genome ind)
                       (sequence-similarity (:genome ind) (:parent2-genome ind))
                       0.0))
              ;
              (= cat :parent2-irrelevance)
              (if (and (:parent1-genome ind) (:parent2-genome ind))
                (let [similarity-to-p2 (sequence-similarity (:genome ind) (:parent2-genome ind))
                      p1-similarity-to-p2 (sequence-similarity (:parent1-genome ind) (:parent2-genome ind))]
                   (- 1.0 (max 0.0 (- similarity-to-p2 p1-similarity-to-p2))))
                1.0)
              ;
              (= cat :reproductive-convergence)
              (if (and (:parent1-genome ind) (:parent2-genome ind))
                (let [g (:genome ind)
                      child1-genome (produce-child-genome-by-autoconstruction g g argmap)
                      child2-genome (produce-child-genome-by-autoconstruction g g argmap)]
                  (max (sequence-similarity g child1-genome)
                       (sequence-similarity g child2-genome)
                       (sequence-similarity child1-genome child2-genome)))
                1.0)
              (= cat :favoritism)
              (if (and (:parent1-genome ind) (:parent2-genome ind))
                (math/abs (- (sequence-similarity (:genome ind) (:parent1-genome ind))
                             (sequence-similarity (:genome ind) (:parent2-genome ind))))
                1.0)
              ;
              (= cat :autoconstruction-blindness)
              (if (some (fn [instruction-map]
                          (and (not (:silent instruction-map))
                               (some #{(:instruction instruction-map)}
                                     #{'genome_autoconstructing 'genome_if_autoconstructing})))
                        (:genome ind))
                0
                1)
              ;
              (= cat :static-size)
              (if (:parent1-genome ind)
                (if (= (count (:genome ind))
                       (count (:parent1-genome ind)))
                  1
                  0)
                1)
              ;
              (= cat :static-instruction-set)
              (if (:parent1-genome ind)
                (let [instruction-set (fn [genome]
                                        (hash-set (keys (frequencies (map :instruction genome)))))]
                  (if (= (instruction-set (:genome ind))
                         (instruction-set (:parent1-genome ind)))
                    1
                    0))
                1)
              ;
              :else (throw (Exception. (str "Unrecognized meta category: " cat)))))]
      (assoc ind :meta-errors (vec (flatten (mapv meta-error-fn meta-error-categories)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluate individuals

(defn compute-total-error
  [errors]
  (reduce +' errors))

(defn compute-root-mean-square-error
  [errors]
  (math/sqrt (/ (apply +' (map #(* % %)
                               errors))
                (count errors))))

(defn compute-hah-error
  [errors]
  (reduce +' (doall (map (fn [rate e] (*' (- 1.01 rate) e))
                         @solution-rates
                         errors))))

(defn normalize-errors
  "Normalizes errors to [0,1] if normalize isn't :none."
  [errors normalization max-error]
  (if (= normalization :none)
    errors
    (map (fn [err]
           (case normalization
             :divide-by-max-error (double (if (>= err max-error)
                                            1.0
                                            (/ err max-error)))
             :e-over-e-plus-1 (double (/ err (inc err)))
             (throw (Exception. (str "Unrecognized argument for normalization: "
                                     normalization)))))
         errors)))

(defn evaluate-individual
  "Returns the given individual with errors, total-errors, and weighted-errors,
   computing them if necessary."
  ([i error-function rand-gen]
    (evaluate-individual i error-function rand-gen
                         {:reuse-errors true
                          :print-history false
                          :total-error-method :sum
                          :normalization :none
                          :max-error 1000}))
  ([i error-function rand-gen
    {:keys [reuse-errors print-history total-error-method normalization max-error]
     :as argmap}]
    (random/with-rng rand-gen
      (let [p (:program i)
            evaluated-i (if (or (not reuse-errors)
                                (nil? (:errors i)))
                         (error-function i)
                         i)
            raw-errors (:errors evaluated-i)
            e (vec (if (and reuse-errors (not (nil? (:errors i))))
                     (:errors i)
                     (do
                       (swap! evaluations-count inc)
                       (normalize-errors raw-errors normalization max-error))))
            te (if (and reuse-errors (not (nil? (:total-error i))))
                 (:total-error i)
                 (compute-total-error raw-errors))
            ne (if (and reuse-errors (not (nil? (:normalized-error i))))
                 (:normalized-error i)
                 (compute-total-error e))
            we (case total-error-method
                 :sum nil
                 :ifs nil
                 :hah (compute-hah-error e)
                 :rmse (compute-root-mean-square-error e)
                 nil)
            new-ind (assoc evaluated-i ; Assign errors and history to i
                           :errors e
                           :total-error te
                           :weighted-error we
                           :normalized-error ne
                           :history (if print-history (cons e (:history i)) (:history i)))]
        new-ind))))




