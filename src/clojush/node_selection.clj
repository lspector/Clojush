(ns clojush.node-selection
  (:use [clojush.util]
        [clojush.random]
        [clojush.globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node selection functions

(defn choose-node-index-with-leaf-probability
  "Returns an index into tree, choosing a leaf with probability 
   node-selection-leaf-probability."
  [tree node-selection-leaf-probability]
  (if (seq? tree)
    (if (> (lrand) node-selection-leaf-probability)
      (second (lrand-nth (filter #(seq? (first %)) (map #(list %1 %2) (all-items tree) (iterate inc 0)))))
      (let [indexed-leaves (filter #(not (seq? (first %))) (map #(list %1 %2) (all-items tree) (iterate inc 0)))]
        (if (empty? indexed-leaves) 0 (second (lrand-nth indexed-leaves)))))
    0))

(defn choose-node-index-by-tournament
  "Returns an index into tree, choosing the largest subtree found in 
   a tournament of size node-selection-tournament-size."
  [tree node-selection-tournament-size]
  (let [c (count-points tree)
        tournament-set
        (for [_ (range node-selection-tournament-size)]
          (let [point-index (lrand-int c)
                subtree-size (count-points (code-at-point tree point-index))]
            {:i point-index :size subtree-size}))]
    (:i (last (sort-by :size tournament-set)))))

(defn select-node-index
  "Returns an index into tree using the node selection method indicated
   by node-selection-method."
  [tree {:keys [node-selection-method
                node-selection-tournament-size
                node-selection-leaf-probability]}]
  (cond 
    (= node-selection-method :unbiased) (lrand-int (count-points tree))
    (= node-selection-method :leaf-probability) (choose-node-index-with-leaf-probability tree node-selection-leaf-probability)
    (= node-selection-method :size-tournament) (choose-node-index-by-tournament tree node-selection-tournament-size)
    :else (throw (Exception. (str ":node-selection-method set to unrecognized value " node-selection-method)))))
