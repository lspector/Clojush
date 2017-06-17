(ns clojush.pushgp.selection.novelty
  (:use [clojush random util globals])
  (:require [clojure.math.numeric-tower :as math]))

(defn select-individuals-for-novelty-archive
  "Returns a number of individuals to be added to the novelty archive. Number
   of indviduals are :individuals-for-novelty-archive-per-generation."
  [population argmap]
  (take (:individuals-for-novelty-archive-per-generation argmap)
        (lshuffle population)))

(defn behavioral-distance ;REF Note: will have to add something for other output types, such as vectors of different types of things
  "Takes two behavior vectors and finds the distance between them. Differences in
   vectors are based on the data type(s) they contain. Distance metric is based on
   the arg :novelty-distance-metric.
   Note that there is no limit on behavior differences, which will only be limited
   by their max bounds based on things like maximum integer size."
  [behavior1 behavior2 {:keys [novelty-distance-metric] :as argmap}]
  (if (= novelty-distance-metric :hamming) ; This is hear, instead of below, for speed reasons
    (apply + (map #(if (= %1 %2) 0 1)
                  behavior1 behavior2))
    (let [behavior-differences (map (fn [b1 b2]
                                      (cond
                                        ; Handles equal behaviors, including if both are :no-stack-item
                                        (= b1 b2) 0
                                        ; If one has :no-stack-item and the other does not, give max difference
                                        (or (= b1 :no-stack-item)
                                            (= b2 :no-stack-item)) max-number-magnitude
                                        :else (case (recognize-literal b1)
                                                :string (levenshtein-distance b1 b2)
                                                (:integer :float) (math/abs (-' b1 b2))
                                                (:boolean :char) (if (= b1 b2) 0 1)
                                                (throw (Exception. (str "Unrecognized behavior type in novelty distance for: " b1))))))
                                    behavior1
                                    behavior2)]
      (case novelty-distance-metric
        :manhattan (apply +' behavior-differences)
        :euclidean (math/sqrt (apply +' (map #(*' % %) behavior-differences)))))))

(defn calculate-behavior-distance-map
  "Calculates a map storing the distances between any two behaviors, of the form:
    {behavior1 {behavior1 dist11 behavior2 dist12 behavior3 dist13 ...}
     behavior2 {behavior1 dist21 behavior22 dist2 behavior3 dist23 ...}
     ...}
   Note: Only has outer-level keys for population behaviors, not archive behaviors.
   But, has inner-level keys for both population and archive behaviors."
  [distinct-pop-behaviors distinct-pop-and-archive-behaviors argmap]
  (loop [behavior (first distinct-pop-behaviors)
         pop-behaviors distinct-pop-behaviors
         behavior-distance-map {}]
    (if (empty? pop-behaviors)
      behavior-distance-map
      (let [distances-from-behavior
            (into {}
                  (map (fn [other-behavior]
                         (vector other-behavior
                                 (if (contains? behavior-distance-map other-behavior)
                                   (get-in behavior-distance-map
                                           [other-behavior behavior])
                                   (behavioral-distance behavior other-behavior argmap))))
                       distinct-pop-and-archive-behaviors))]
        (recur (first (rest pop-behaviors))
               (rest pop-behaviors)
               (assoc behavior-distance-map behavior distances-from-behavior))))))

(defn calculate-behavior-sparseness
  "Calculates the sparseness/novelty of an individual by averaging together the
   distances between it and its k nearest neighbors. First, it must look up those
   distances using the behavior-distance-map."
  [pop-and-archive-behaviors behavior-distance-map {:keys [novelty-number-of-neighbors-k]}]
  (let [behavior-distances-to-others
        (into {}
              (for [[behavior dist-map] behavior-distance-map]
                (vector behavior
                        (map dist-map pop-and-archive-behaviors))))]
    (into {}
          (for [[behavior distances] behavior-distances-to-others]
            (vector behavior
                    (/ (apply +'
                              (take novelty-number-of-neighbors-k
                                    (sort distances)))
                       novelty-number-of-neighbors-k))))))

(defn assign-novelty-to-individual
  "Calculates the novelty of the individual based on the behaviors in the population
   and in the novelty-archive. Returns the individual with the :novelty key set, and
   if :novelty is a meta-error-category, also sets that."
  [individual behavior-sparseness {:keys [meta-error-categories]}]
  (let [novelty (get behavior-sparseness (:behaviors individual))]
    (assoc individual
           :novelty novelty
           :meta-errors (replace {:novelty novelty} (:meta-errors individual)))))

(defn calculate-novelty
  "Calculates novelty for each individual in the population with respect to the
   rest of the population and the novelty-archive. Sets novelty to meta-error
   if necessary."
  [pop-agents novelty-archive {:keys [use-single-thread] :as argmap}]
  (print "Calculating novelty...") (flush)
  (let [pop-behaviors (map #(:behaviors (deref %)) pop-agents)
        pop-and-archive-behaviors (concat pop-behaviors
                                          (map :behaviors novelty-archive))
        behavior-distance-map (calculate-behavior-distance-map (distinct pop-behaviors)
                                                               (distinct pop-and-archive-behaviors)
                                                               argmap)
        behavior-sparseness (calculate-behavior-sparseness pop-and-archive-behaviors
                                                           behavior-distance-map
                                                           argmap)]
    (dorun (map #((if use-single-thread swap! send)
                  % assign-novelty-to-individual behavior-sparseness argmap)
                pop-agents)))
  (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
  (println "Done calculating novelty.")
  (println "\nNovelty Numbers:" (sort > (map #(float (:novelty (deref %))) pop-agents))))

(defn novelty-tournament-selection
  "Returns an individual that does the best out of a tournament based on novelty."
  [pop {:keys [tournament-size] :as argmap}]
  (let [tournament-set (doall (for [_ (range tournament-size)]
                                (lrand-nth pop)))]
    (reduce (fn [i1 i2] (if (> (:novelty i1) (:novelty i2)) i1 i2))
            tournament-set)))
