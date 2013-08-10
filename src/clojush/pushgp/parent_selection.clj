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
;; condensed lexicase selection

(defn but-nth
  [seq n]
  (concat (take n seq)
          (drop (inc n) seq)))

(defn without-nths
  [seq n-coll]
  (loop [result (vec seq)
         remaining-indices n-coll]
    (if (empty? remaining-indices)
      (remove not result)
      (recur (assoc result (first remaining-indices) nil)
             (rest remaining-indices)))))

;(defn lexicase-reachable
;  "Returns true if an individual with error vector ev could be reached by lexicase selection
;in a population with error vectors evs."
;  [ev evs]
;  (println (count evs) ev)
;  (println (apply map == evs))
;  (if (or (empty? ev) (every? identity (apply map == evs)))
;    (do (println true) true)
;    (let [ev-elite-cases 
;          (filter identity
;                  (for [c (range (count ev))]
;                    (if (== (nth ev c) (apply min (map #(nth % c) evs)))
;                      c
;                      nil)))]
;      (println ev-elite-cases)
;      (if (= (count ev-elite-cases) (count ev))
;        (do (println true) true)
;        (if (empty? ev-elite-cases)
;          (do (println false) false)
;          (some (fn [c] 
;                  (lexicase-reachable 
;                    (but-nth ev c)
;                    (map #(but-nth % c) 
;                         (filter #(== (nth % c) (nth ev c))
;                                 evs))))
;                ev-elite-cases))))))



(defn lexicase-reachable
  "Returns true if an individual with error vector ev could be reached by lexicase selection
in a population with error vectors evs."
  [ev evs]
  ;(println (count evs) ev)
  (if (or (empty? ev) (<= (count evs) 1))
    true ;(do (println true) true)
    (let [non-distinguishing-cases (filter 
                                     (fn [c] (apply == (map #(nth % c) evs)))
                                     (range (count ev)))
          ev-reduced (without-nths ev non-distinguishing-cases)
          evs-reduced (map #(without-nths % non-distinguishing-cases) evs)]
      (if (empty? ev-reduced) 
        true ;(do (println true) true)
        (let [ev-elite-cases 
              (filter (fn [c]
                        (== (nth ev-reduced c) 
                            (apply min (map #(nth % c) evs-reduced))))
                      (range (count ev-reduced)))]
          ;(println ev-elite-cases)
          (if (= (count ev-elite-cases) (count ev-reduced))
            true ;(do (println true) true)
            (if (empty? ev-elite-cases)
              false ;(do (println false) false)
              (some (fn [c] 
                      (lexicase-reachable 
                        (but-nth ev-reduced c)
                        (distinct
                          (map #(but-nth % c) 
                               (filter #(== (nth % c) (nth ev-reduced c))
                                       evs-reduced)))))
                    ev-elite-cases))))))))

;(defn lexicase-reachable
;  "Returns true if an individual with error vector ev could be reached by lexicase selection
;in a population with error vectors evs."
;  [ev evs]
;  ;(println (count evs) ev)
;  (if (or (empty? ev) (<= (count evs) 1))
;    true ;(do (println true) true)
;    (let [non-distinguishing-cases (filter 
;                                     (fn [c] (apply == (map #(nth % c) evs)))
;                                     (range (count ev)))
;          ev-reduced (without-nths ev non-distinguishing-cases)
;          evs-reduced (map #(without-nths % non-distinguishing-cases) evs)]
;      (if (empty? ev-reduced) 
;        true ;(do (println true) true)
;        (let [ev-elite-cases 
;              (filter (fn [c]
;                        (== (nth ev-reduced c) 
;                            (apply min (map #(nth % c) evs-reduced))))
;                      (range (count ev-reduced)))]
;          ;(println ev-elite-cases)
;          (if (= (count ev-elite-cases) (count ev-reduced))
;            true ;(do (println true) true)
;            (if (empty? ev-elite-cases)
;              false ;(do (println false) false)
;              (lexicase-reachable 
;                (without-nths ev-reduced ev-elite-cases)
;                (distinct 
;                  (map #(without-nths % ev-elite-cases)
;                       (filter (fn [v] 
;                                 (every? #(== (nth v %) (nth ev-reduced %))
;                                         ev-elite-cases))
;                               evs-reduced)))))))))))
  
  
  
(defn extract-lexicase-reachable-subsets
  "Builds a set of all of the lexicase-reachable sets of individuals in the population."
  [pop-agents]
  (println "Extracting lexicase-reachable subsets...")
  (reset! lexicase-reachable-subsets
          (let [candidate-sets (map vec (vals (group-by :errors (map deref pop-agents))))
                representative-error-vectors (map :errors (map first candidate-sets))]
            (vec (filter #(lexicase-reachable (:errors (first %)) representative-error-vectors)
                         candidate-sets))))
  (println "Extracted" (count @lexicase-reachable-subsets) 
           "subsets including" (count (distinct (flatten @lexicase-reachable-subsets)))
           "individuals grouped as" (map count @lexicase-reachable-subsets)
           ;(map :errors (map first @lexicase-reachable-subsets))
           ))

(defn condensed-lexicase-selection
  "Returns an individual produced by condensed lexicase selection."
  []
  (lrand-nth (lrand-nth @lexicase-reachable-subsets)))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection

(defn select
  "Returns a selected parent."
  [pop tournament-size radius location]
  (cond @global-use-lexicase-selection (lexicase-selection pop)
        @global-use-condensed-lexicase-selection (condensed-lexicase-selection)
        :else ;; use tournament selection by default
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
