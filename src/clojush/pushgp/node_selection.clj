(ns clojush.pushgp.node-selection
  (:use [clojush.util]
        [clojush.random]
        [clojush.globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node selection functions

(defn choose-node-index-with-leaf-probability
  "Returns an index into tree, choosing a leaf with probability 
   @global-node-selection-leaf-probability."
  [tree]
  (if (seq? tree)
    (if (> (lrand) @global-node-selection-leaf-probability)
      (second (lrand-nth (filter #(seq? (first %)) (map #(list %1 %2) (all-items tree) (iterate inc 0)))))
      (let [indexed-leaves (filter #(not (seq? (first %))) (map #(list %1 %2) (all-items tree) (iterate inc 0)))]
        (if (empty? indexed-leaves) 0 (second (lrand-nth indexed-leaves)))))
    0))

(defn choose-node-index-by-tournament
  "Returns an index into tree, choosing the largest subtree found in 
   a tournament of size @global-node-selection-tournament-size."
  [tree]
  (let [c (count-points tree)
        tournament-set
        (for [_ (range @global-node-selection-tournament-size)]
          (let [point-index (lrand-int c)
                subtree-size (count-points (code-at-point tree point-index))]
            {:i point-index :size subtree-size}))]
    (:i (last (sort-by :size tournament-set)))))

(defn select-node-index
  "Returns an index into tree using the node selection method indicated
   by @global-node-selection-method."
  [tree]
  (let [method @global-node-selection-method]
    (cond 
      (= method :unbiased) (lrand-int (count-points tree))
      (= method :leaf-probability) (choose-node-index-with-leaf-probability tree)
      (= method :size-tournament) (choose-node-index-by-tournament tree))))