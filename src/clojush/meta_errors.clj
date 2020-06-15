(ns clojush.meta-errors
  (:use [clojush util pushstate random globals individual simplification]
        clojush.pushgp.genetic-operators)
  (:require [clojure.math.numeric-tower :as math]
            [clj-random.core :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate meta-errors

(defn meta-error-fn-from-keyword
  "Takes a keyword and returns the meta-error function with the corresponding name."
  [k]
  (try
    (var-get (get (ns-publics 'clojush.meta-errors)
                  (read-string (str (name k) "-meta-error"))))
    (catch NullPointerException e
      (throw (NullPointerException. (str "No meta-error function for keyword " k))))))

(defn evaluate-individual-meta-errors
  "Calculates one meta-error for each meta-error category provided. Each
  meta-error-category should either be a function (which must be namespace-qualified
  if provided in a command-line argument) or a keyword corresponding to a pre-defined 
  meta-error function. In either case the function should take an individual, an evaluated 
  population, and an argmap, and it should return a numeric meta error value or collection 
  of values, for which lower is interpreted as better. For keyword :foo, the corresponding 
  meta-error function will be clojush.meta-errors/foo-meta-error."
  [ind evaluated-population argmap]
  (assoc ind :meta-errors
             (vec (flatten
                    (for [cat (:meta-error-categories argmap)]
                      (if (fn? cat)
                        (cat ind evaluated-population argmap)
                        (let [f (meta-error-fn-from-keyword cat)]
                          (if (fn? f)
                            (f ind evaluated-population argmap)
                            (throw (Exception. (str "Unrecognized meta category: " cat)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; meta-error functions

(defn size-meta-error
  [ind evaluated-population argmap]
  (count (:genome ind)))

; :compressibility ;;TMH fix later


; Meta-errors related to modularity:
(defn reuse-meta-error
  [ind evaluated-population argmap]
  (- 0 (if (seq? (:reuse-info ind))
         (mean (:reuse-info ind))
         (:reuse-info ind)
         )))

(defn repetition-meta-error
  [ind evaluated-population argmap]
  (if (seq? (:repetition-info ind))
    (mean (:repetition-info ind))
    (:repetition-info ind)
    ))

(defn total-error-meta-error
  [ind evaluated-population argmap]
  (:total-error ind))

(defn unsolved-cases-meta-error
  [ind evaluated-population argmap]
  (count (filter #(> % (:error-threshold argmap))
                 (:errors ind))))

(defn rand-meta-error
  [ind evaluated-population argmap]
  (rand))

(defn rand-bit-meta-error
  [ind evaluated-population argmap]
  (rand-nth [0 1]))

(defn age-meta-error
  [ind evaluated-population argmap]
  (:age ind))

(defn newborn-meta-error
  [ind evaluated-population argmap]
  (if (zero? (:age ind)) 1 0))

(defn empty-genome-meta-error
  [ind evaluated-population argmap]
  (if (empty? (:genome ind))
    1
    0))

(defn novelty-meta-error
  "Novelty was calculated earlier and stored in each individual.
  Note that we need to invert novelty, since it is calculated as a value to maximize."
  [ind evaluated-population argmap]
  (/ 1 (inc (:novelty ind))))

(defn novelty-by-case-meta-error
  "Novelty-by-case was calculated earlier and stored in each individual."
  [ind evaluated-population argmap]
  (:novelty-by-case ind))

(defn amorphousness-meta-error
  [ind evaluated-population argmap]
  (- 1 (double (/ (count-parens (:program ind))
                  (count-points (:program ind))))))         ;Number of (open) parens / points


(defn gens-since-total-error-change-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :gens-since-total-error-change"))
    (let [hist (mapv (partial reduce +) (:history ind))]
      (if (or (empty? hist)
              (apply = hist))
        1000000
        (dec (count (take-while #(= % (first hist)) (rest hist))))))))

(defn gens-since-error-change-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :gens-since-error-change"))
    (let [hist (:history ind)]
      (if (or (empty? hist)
              (apply = hist))
        1000000
        (dec (count (take-while #(= % (first hist)) (rest hist))))))))

(defn no-recent-error-change-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :no-recent-error-change"))
    (let [hist (:history ind)
          limit (:error-change-recency-limit argmap)]
      (if (< (count hist) limit)
        1
        (if (apply = (take limit hist))
          2
          0)))))

;(defn lineage-redundancy-meta-error
;  [ind evaluated-population argmap]
;  (if (not (:print-history argmap))
;    (throw
;     (Exception.
;      ":print-history must be true for :lineage-redundancy"))
;    (let [hist (:history ind)]
;      (if (< (count hist) 2)
;        1/2
;        (- 1 (/ (count (distinct hist))
;                (count hist)))))))

(defn lineage-redundancy-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :lineage-redundancy"))
    (let [hist (if (:lineage-redundancy-window argmap)
                 (take (:lineage-redundancy-window argmap) (:history ind))
                 (:history ind))]
      (/ (- (count hist)
            (count (distinct hist)))
         (count hist)))))

(defn redundant-lineage-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :redundant-lineage"))
    (let [hist (:history ind)]
      (if (< (count hist) 2)
        0
        (if (> (/ (count (distinct hist))
                  (count hist))
               1/2)
          0
          1)))))

(defn resilience-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :resilience"))
    (let [hist (take 5 (:history ind))]
      (if (< (count hist) 2)
        0
        (if (> (/ (count (distinct hist))
                  (count hist))
               2/5)
          0
          1)))))

(defn stasis-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :stasis"))
    (let [hist (:history ind)]
      (if (< (count hist) 2)
        0
        (if (some (fn [[new-err old-err]]
                    (and (not (zero? old-err))
                         (not= new-err old-err)))
                  (map vector (first hist) (second hist)))
          0
          1)))))

(defn case-stasis-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :case-stasis"))
    (if (empty? (rest (:history ind)))
      (vec (repeat (count (:errors ind)) 0))
      (vec (for [case-history (apply map list (:history ind))]
             (if (or (zero? (first case-history))
                     (and (not (zero? (second case-history)))
                          (not (= (first case-history)
                                  (second case-history)))))
               0
               1))))))

(defn non-improvement-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :non-improvement"))
    (let [hist (:history ind)]
      (if (< (count hist) 2)
        0
        (if (some (fn [[new-err old-err]]
                    (< new-err old-err))
                  (map vector (first hist) (second hist)))
          0
          1)))))

(defn repeating-lineage-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :repeating-lineage"))
    (let [hist (:history ind)]
      (if (< (count hist) 2)
        0
        (if (apply distinct? hist)
          0
          1)))))

(defn gens-since-total-error-improvement-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
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
          (count (take-while #(>= % 0) diffs)))))))

(defn total-error-improvement-ratio-meta-error
  [ind evaluated-population argmap]
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
                (count diffs)))))))

(defn total-error-new-best-ratio-meta-error
  [ind evaluated-population argmap]
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
                        0)))))))))

(defn discounted-total-error-new-best-ratio-meta-error
  [ind evaluated-population argmap]
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
                   (+ max-total (/ 1 scale)))))))))

(defn discounted-total-error-improvement-meta-error
  [ind evaluated-population argmap]
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
          (- 1.0 sum))))))

(defn case-stagnation-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :case-stagnation"))
    (if (empty? (rest (:history ind)))
      (vec (repeat (count (:errors ind)) 1000000))
      (vec (for [case-history (apply map list (:history ind))]
             (if (zero? (first case-history))
               -100000                                      ;; solved, improvement doesn't matter
               (let [improvements (mapv (fn [[newer-error older-error]]
                                          (if (< newer-error older-error)
                                            1.0
                                            (if (= newer-error older-error)
                                              -1.0
                                              0.0)))
                                        (partition 2 1 case-history))
                     weights (take (count improvements)
                                   (iterate (partial *
                                                     (- 1 (:improvement-discount argmap)))
                                            1))
                     sum (reduce + (mapv * improvements weights))]
                 (- sum))))))))

(defn case-gens-since-improvement-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :case-gens-since-improvement"))
    (let [huge 1000000]
      (if (empty? (rest (:history ind)))
        (vec (repeat (count (:errors ind)) huge))
        (vec (for [case-history (apply map list (:history ind))]
               (if (zero? (first case-history))
                 (- huge)                                   ;; solved, improvement doesn't matter
                 (let [improved? (mapv (fn [[newer-error older-error]]
                                         (< newer-error older-error))
                                       (partition 2 1 case-history))
                       gens (take-while not improved?)]
                   (if (= (count gens)
                          (count improved?))
                     huge
                     (count gens))))))))))

(defn case-gens-since-change-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :case-gens-since-change"))
    (let [huge 1000000]
      (if (empty? (rest (:history ind)))
        (vec (repeat (count (:errors ind)) huge))
        (vec (for [case-history (apply map list (:history ind))]
               (if (zero? (first case-history))
                 (- huge)                                   ;; solved, improvement doesn't matter
                 (let [changed? (mapv (fn [[newer-error older-error]]
                                        (not= newer-error older-error))
                                      (partition 2 1 case-history))
                       gens (take-while not changed?)]
                   (if (= (count gens)
                          (count changed?))
                     huge
                     (count gens))))))))))

(defn gens-since-change-0-if-solved-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :gens-since-change-0-if-solved"))
    (let [huge 1000000]
      (if (empty? (rest (:history ind)))
        (vec (repeat (count (:errors ind)) huge))
        (vec (for [case-history (apply map list (:history ind))]
               (if (zero? (first case-history))
                 0                                          ;; solved, error 0
                 (let [changed? (mapv (fn [[newer-error older-error]]
                                        (not= newer-error older-error))
                                      (partition 2 1 case-history))
                       gens (take-while not changed?)]
                   (if (= (count gens)
                          (count changed?))
                     huge
                     (count gens))))))))))

(defn case-sibling-uniformity-meta-error
  [ind evaluated-population argmap]
  (if (empty? (:parent-uuids ind))
    (vec (repeat (count (:errors ind)) 1))
    (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                    (first (:parent-uuids %)))
                                 (not (:is-random-replacement %))) ; new random sibs don't count
                           evaluated-population)]
      (vec (for [case-index (range (count (:errors ind)))]
             (if (zero? (nth (:errors ind) case-index))     ;; solved
               0
               (if (or (empty? siblings)
                       (apply = (mapv #(nth (:errors %) case-index)
                                      siblings)))
                 1
                 0)))))))

(defn case-family-uniformity-meta-error
  [ind evaluated-population argmap]
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
               (if (zero? (nth (:errors ind) case-index))   ;; solved
                 0
                 (if
                   (some (fn [sib]
                           (or (zero? (-> (:history sib)    ;; sib solved
                                          (first)
                                          (nth case-index)))
                               (not= (-> (:history sib)     ;; sib error different than mom's
                                         (first)
                                         (nth case-index))
                                     (-> (:history sib)
                                         (second)
                                         (nth case-index)))))
                         siblings)
                   1
                   2))))))))

(defn mom-doesnt-change-behavior-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :mom-doesnt-change-behavior"))
    (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                    (first (:parent-uuids %)))
                                 (not (empty? (rest (:history %))))) ; new random sibs don't count
                           evaluated-population)]
      (vec (for [case-index (range (count (:errors ind)))]
             (if (zero? (nth (:errors ind) case-index))
               0                                            ;; solved
               (if (or (empty? (:parent-uuids ind))
                       (empty? (rest (:history ind))))
                 2                                          ;; not solved, no mom who might have produced diff behaviors
                 (if
                   (some (fn [sib]
                           (not= (-> (:history sib)         ;; sib error different than mom's
                                     (first)
                                     (nth case-index))
                                 (-> (:history sib)
                                     (second)
                                     (nth case-index))))
                         siblings)
                   1                                        ;; not solved, mom did produce diff behaviors
                   3                                        ;; not solved, mom didn't produce diff behaviors
                   ))))))))

(defn case-appropriate-family-uniformity-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :case-appropriate-family-uniformity"))
    (if (or (empty? (:parent-uuids ind))
            (empty? (rest (:history ind))))
      (vec (repeat (count (:errors ind)) 1))
      (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                      (first (:parent-uuids %)))
                                   (not (empty? (rest (:history %))))) ; new random sibs don't count
                             evaluated-population)]
        (vec (for [case-index (range (count (:errors ind)))]
               (if (zero? (nth (second (:history ind)) case-index))
                 (if                                        ;; mom solved, error for any child to be different
                   (some (fn [sib]
                           (not= (-> (:history sib)         ;; sib error different than mom's
                                     (first)
                                     (nth case-index))
                                 (-> (:history sib)
                                     (second)
                                     (nth case-index))))
                         siblings)
                   1
                   0)
                 (if                                        ;; mom unsolved, error if there's not a different child
                   (some (fn [sib]
                           (not= (-> (:history sib)         ;; sib error different than mom's
                                     (first)
                                     (nth case-index))
                                 (-> (:history sib)
                                     (second)
                                     (nth case-index))))
                         siblings)
                   0
                   1))))))))

(defn case-appropriate-family-diversity-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :case-appropriate-family-diversity"))
    (if (or (empty? (:parent-uuids ind))
            (empty? (rest (:history ind))))
      (vec (repeat (count (:errors ind)) 1))
      (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                      (first (:parent-uuids %)))
                                   (not (empty? (rest (:history %))))) ; new random sibs don't count
                             evaluated-population)]
        (vec (for [case-index (range (count (:errors ind)))]
               (if (zero? (nth (second (:history ind)) case-index))
                 (if                                        ;; mom solved, error for any child to be different
                   (some (fn [sib]
                           (not= (-> (:history sib)         ;; sib error different than mom's
                                     (first)
                                     (nth case-index))
                                 (-> (:history sib)
                                     (second)
                                     (nth case-index))))
                         siblings)
                   1
                   0)
                 ;; mom unsolved, higher error the fewer variants
                 (let [num-variants (count (distinct (map #(nth (first (:history %)) case-index)
                                                          siblings)))]
                   (/ 1 num-variants)))))))))

(defn case-scaled-error-plus-change-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-errors ind)
           (:parent2-errors ind))
    (vec (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                 (:errors ind)
                                 (:parent1-errors ind)
                                 (:parent2-errors ind))]
           (+' (*' e 1000000)
               (#(cond
                   (neg? %) -1
                   (pos? %) 1
                   :else 0)
                 (- e (min p1e p2e))))))
    (mapv #(*' % 1000000) (:errors ind))))

(defn case-family-non-improvement-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :case-family-non-improvement"))
    (if (or (empty? (:parent-uuids ind))
            (empty? (rest (:history ind))))
      (vec (repeat (count (:errors ind)) 1))
      (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                      (first (:parent-uuids %)))
                                   (not (empty? (rest (:history %))))) ; new random sibs don't count
                             evaluated-population)]
        (vec (for [case-index (range (count (:errors ind)))]
               (if (zero? (nth (:errors ind) case-index))   ;; solved
                 0
                 (if
                   (some (fn [sib]
                           (or (zero? (-> (:history sib)    ;; sib solved
                                          (first)
                                          (nth case-index)))
                               (< (-> (:history sib)        ;; sib error better than mom's
                                      (first)
                                      (nth case-index))
                                  (-> (:history sib)
                                      (second)
                                      (nth case-index)))))
                         siblings)
                   1
                   2))))))))

(defn case-family-non-improvement-or-uniformity-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :case-family-non-improvement-or-uniformity"))
    (if (or (empty? (:parent-uuids ind))
            (empty? (rest (:history ind))))
      (vec (repeat (count (:errors ind)) 1))
      (let [siblings (filter #(and (= (first (:parent-uuids ind))
                                      (first (:parent-uuids %)))
                                   (not (empty? (rest (:history %))))) ; new random sibs don't count
                             evaluated-population)]
        (vec (for [case-index (range (count (:errors ind)))]
               (if (zero? (nth (:errors ind) case-index))   ;; solved
                 0
                 (if
                   (some (fn [sib]
                           (or (zero? (-> (:history sib)    ;; sib solved
                                          (first)
                                          (nth case-index)))
                               (< (-> (:history sib)        ;; sib error better than mom's
                                      (first)
                                      (nth case-index))
                                  (-> (:history sib)
                                      (second)
                                      (nth case-index)))))
                         siblings)
                   1
                   (if (some (fn [sib]
                               (not= (-> (:history sib)     ;; sib error different than mom's
                                         (first)
                                         (nth case-index))
                                     (-> (:history sib)
                                         (second)
                                         (nth case-index))))
                             siblings)
                     2
                     3)))))))))

(defn case-family-certainty-meta-error
  [ind evaluated-population argmap]
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
               (if (zero? (nth (:errors ind) case-index))   ;; solved
                 0
                 (/ 1 (count
                        (distinct
                          (for [s siblings]
                            (nth (first (:history s))
                                 case-index))))))))))))

(defn family-uniformity-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :family-uniformity"))
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
          1)))))

(defn repeated-errors-meta-error
  [ind evaluated-population argmap]
  (if (not (:print-history argmap))
    (throw
      (Exception.
        ":print-history must be true for :repeated-errors"))
    (if (empty? (rest (:history ind)))
      (vec (repeat (count (:errors ind)) 2))
      (vec (for [case-index (range (count (:errors ind)))]
             (if (zero? (nth (:errors ind) case-index))     ;; solved
               0
               (let [latest (nth (first (:history ind)) case-index)
                     olders (map #(nth % case-index) (rest (:history ind)))]
                 (if (some #{latest} (set olders))
                   3
                   1))))))))

(defn error-favoritism-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-errors ind) (:parent2-errors ind))
    (let [all (map list
                   (:errors ind)
                   (:parent1-errors ind)
                   (:parent2-errors ind))
          diff-parents (filter (fn [[e p1e p2e]] (not= p1e p2e)) all)
          from-p1 (count (filter (fn [[e p1e p2e]] (= e p1e)) diff-parents))
          from-p2 (count (filter (fn [[e p1e p2e]] (= e p2e)) diff-parents))]
      (if (empty? diff-parents)
        0
        (/ (Math/abs (- from-p1 from-p2))
           (count diff-parents))))
    1))

(defn case-error-neglect-meta-error
  [ind evaluated-population argmap]
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
    (vec (repeat (count (:errors ind)) 6))))

(defn devolution-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-errors ind)
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
    (vec (repeat (count (:errors ind)) 6))))

(defn error-neglect-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-errors ind) (:parent2-errors ind))
    (reduce + (for [[e p1e p2e] (mapv #(vector %1 %2 %3)
                                      (:errors ind)
                                      (:parent1-errors ind)
                                      (:parent2-errors ind))]
                (if (<= e (min p1e p2e))
                  0
                  1)))
    (count (:errors ind))))

(defn case-family-variation-meta-error
  [ind evaluated-population argmap]
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
               (if (zero? (nth (:errors ind) case-index))   ;; solved
                 0
                 (if
                   (some (fn [sib]                          ;; sibling solved
                           (zero? (-> (:history sib)
                                      (first)
                                      (nth case-index))))
                         siblings)
                   0
                   (if (some (fn [sib]                      ;; sib error different than mom's
                               (not= (-> (:history sib)
                                         (first)
                                         (nth case-index))
                                     (-> (:history sib)
                                         (second)
                                         (nth case-index))))
                             siblings)
                     1
                     0)))))))))

(defn washout-mother-meta-error
  [ind evaluated-population argmap]
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
               (if (zero? (nth (:errors ind) case-index))   ;; solved
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
                   1))))))))

(defn clone-distance-meta-error
  [ind evaluated-population argmap]
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
                 (recur (inc step)
                        (cons child genomes)))))))))

(defn reproductive-infidelity-meta-error
  [ind evaluated-population argmap]
  (let [g (:genome ind)]
    (- 1.0
       (sequence-similarity
         g
         (produce-child-genome-by-autoconstruction g g argmap)))))

(defn reproductive-fidelity-meta-error
  [ind evaluated-population argmap]
  (let [g (:genome ind)]
    (sequence-similarity
      g
      (produce-child-genome-by-autoconstruction g g argmap))))

(defn reproductive-inconsistency-meta-error
  [ind evaluated-population argmap]
  (let [g (:genome ind)]
    (- 1.0
       (sequence-similarity
         (produce-child-genome-by-autoconstruction g g argmap)
         (produce-child-genome-by-autoconstruction g g argmap)))))

(defn reproductive-consistency-meta-error
  [ind evaluated-population argmap]
  (let [g (:genome ind)]
    (sequence-similarity
      (produce-child-genome-by-autoconstruction g g argmap)
      (produce-child-genome-by-autoconstruction g g argmap))))

(defn similarity-to-most-similar-parent-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-genome ind) (:parent2-genome ind))
    (max (sequence-similarity (:genome ind) (:parent1-genome ind))
         (sequence-similarity (:genome ind) (:parent2-genome ind)))
    1.0))

(defn difference-from-most-similar-parent-meta-error
  [ind evaluated-population argmap]
  (- 1.0 (if (and (:parent1-genome ind) (:parent2-genome ind))
           (max (sequence-similarity (:genome ind) (:parent1-genome ind))
                (sequence-similarity (:genome ind) (:parent2-genome ind)))
           0.0)))

(defn difference-from-least-similar-parent-meta-error
  [ind evaluated-population argmap]
  (- 1.0 (if (and (:parent1-genome ind) (:parent2-genome ind))
           (min (sequence-similarity (:genome ind) (:parent1-genome ind))
                (sequence-similarity (:genome ind) (:parent2-genome ind)))
           0.0)))

(defn difference-from-mate-meta-error
  [ind evaluated-population argmap]
  (- 1.0 (if (:parent2-genome ind)
           (sequence-similarity (:genome ind) (:parent2-genome ind))
           0.0)))

(defn parent2-irrelevance-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-genome ind) (:parent2-genome ind))
    (let [similarity-to-p2 (sequence-similarity (:genome ind) (:parent2-genome ind))
          p1-similarity-to-p2 (sequence-similarity (:parent1-genome ind) (:parent2-genome ind))]
      (- 1.0 (max 0.0 (- similarity-to-p2 p1-similarity-to-p2))))
    1.0))

(defn reproductive-convergence-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-genome ind) (:parent2-genome ind))
    (let [g (:genome ind)
          child1-genome (produce-child-genome-by-autoconstruction g g argmap)
          child2-genome (produce-child-genome-by-autoconstruction g g argmap)]
      (max (sequence-similarity g child1-genome)
           (sequence-similarity g child2-genome)
           (sequence-similarity child1-genome child2-genome)))
    1.0))

(defn favoritism-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-genome ind)
           (:parent2-genome ind)
           (not= (:parent1-genome ind) (:parent2-genome ind)))
    (/ (math/abs (- (sequence-similarity (:genome ind) (:parent1-genome ind))
                    (sequence-similarity (:genome ind) (:parent2-genome ind))))
       (- 1 (sequence-similarity (:parent1-genome ind) (:parent2-genome ind))))
    1.0))

(defn autoconstruction-blindness-meta-error
  [ind evaluated-population argmap]
  (if (some (fn [instruction-map]
              (and (not (:silent instruction-map))
                   (some #{(:instruction instruction-map)}
                         #{'genome_autoconstructing 'genome_if_autoconstructing})))
            (:genome ind))
    0
    1))

(defn no-genesis-meta-error
  [ind evaluated-population argmap]
  (if (some (fn [instruction-map]
              (and (not (:silent instruction-map))
                   (some #{(:instruction instruction-map)}
                         #{'genome_genesis})))
            (:genome ind))
    0
    1))

(defn static-instruction-set-meta-error
  [ind evaluated-population argmap]
  (if (:parent1-genome ind)
    (let [instruction-set (fn [genome]
                            (hash-set (keys (frequencies (map :instruction genome)))))]
      (if (= (instruction-set (:genome ind))
             (instruction-set (:parent1-genome ind)))
        1
        0))
    1))

(defn inherited-errors-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-errors ind)
           (:parent2-errors ind)
           (not= (:errors ind) (:parent1-errors ind))
           (not= (:errors ind) (:parent2-errors ind)))
    0
    1))

#_(defn inherited-errors-meta-error
    [ind evaluated-population argmap]
    (if (and (:parent1-errors ind)
             (:parent2-errors ind))
      (if (and (not= (:errors ind) (:parent1-errors ind))
               (not= (:errors ind) (:parent2-errors ind)))
        0
        1)
      0))

#_(defn case-inherited-errors-meta-error
    [ind evaluated-population argmap]
    (if (and (:parent1-errors ind)
             (:parent2-errors ind))
      (mapv #(if (some #{%1} [%2 %3]) 1 0)
            (:errors ind)
            (:parent1-errors ind)
            (:parent2-errors ind))
      (vec (repeat (count (:errors ind)) 1))))

(defn case-inherited-errors-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-errors ind)
           (:parent2-errors ind))
    (mapv #(if (some #{%1} [%2 %3]) 1 0)
          (:errors ind)
          (:parent1-errors ind)
          (:parent2-errors ind))
    (vec (repeat (count (:errors ind)) 1))))

(defn case-inherited-non-zero-errors-meta-error
  [ind evaluated-population argmap]
  (if (and (:parent1-errors ind)
           (:parent2-errors ind))
    (mapv #(if (and (not (zero? %1))
                    (some #{%1} [%2 %3]))
             1
             0)
          (:errors ind)
          (:parent1-errors ind)
          (:parent2-errors ind))
    (vec (repeat (count (:errors ind)) 1))))

(defn case-error-frequency-meta-error
  [ind evaluated-population argmap]
  (mapv (fn [e i]
          (count (filter #(= e (nth (:errors %) i))
                         evaluated-population)))
        (:errors ind)
        (iterate inc 0)))
