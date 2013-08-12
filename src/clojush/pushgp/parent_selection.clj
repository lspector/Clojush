(ns clojush.pushgp.parent-selection
  (:use [clojush.random]
        [clojush.globals])
  (:require [clojure.set :as set]))

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
;; elitegroup lexicase selection

;(defn build-elitegroups
;  "Builds a sequence that partitions the cases into sub-sequences, with cases 
;grouped when they produce the same set of elite individuals in the population."
;  [pop-agents]
;  (println "Building case elitegroups...")
;  (let [pop (retain-one-individual-per-error-vector (map deref pop-agents))
;        cases (range (count (:errors (first pop))))
;        elites (map (fn [c]
;                      (let [min-error-for-case 
;                            (apply min (map #(nth % c) (map :errors pop)))]
;                        (filter #(== (nth (:errors %) c) min-error-for-case)
;                                pop)))
;                    cases)]
;    (reset! elitegroups
;            (vals (group-by #(nth elites %) cases)))
;    (println (count @elitegroups) "elitegroups:" @elitegroups)))

(defn build-elitegroups
  "Builds a sequence that partitions the cases into sub-sequences, with cases 
grouped when they produce the same set of elite individuals in the population. 
In addition, if group A has produces population subset PS(A), and group B 
produces population subset PS(B), and PS(A) is a proper subset of PS(B), then 
group B is discarded. "
  [pop-agents]
  (println "Building case elitegroups...")
  (let [pop (retain-one-individual-per-error-vector (map deref pop-agents))
        cases (range (count (:errors (first pop))))
        elites (map (fn [c]
                      (let [min-error-for-case 
                            (apply min (map #(nth % c) (map :errors pop)))]
                        (filter #(== (nth (:errors %) c) min-error-for-case)
                                pop)))
                    cases)
        all-elitegroups (vals (group-by #(nth elites %) cases))
        pruned-elitegroups (filter (fn [eg]
                                     (let [e (set (nth elites (first eg)))]
                                       (some (fn [eg2]
                                               (let [e2 (set (nth elites (first eg2)))]
                                                 (and (not= e e2)
                                                      (set/subset? e2 e))))
                                             all-elitegroups)))
                                   all-elitegroups)]
    (reset! elitegroups pruned-elitegroups)
    (println (count @elitegroups) "elitegroups:" @elitegroups)))

(defn elitegroup-lexicase-selection
  "Returns an individual produced by elitegroup lexicase selection."
  [pop]
  (loop [survivors (retain-one-individual-per-error-vector pop)
         cases (lshuffle (map lrand-nth @elitegroups))]
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
  (cond @global-use-lexicase-selection (lexicase-selection pop)
        @global-use-elitegroup-lexicase-selection (elitegroup-lexicase-selection pop)
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
