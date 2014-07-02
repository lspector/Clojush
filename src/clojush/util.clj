(ns clojush.util
  (:require [clojure.math.numeric-tower :as math]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.string :as string])
  (:use [clojush.globals]
        [clojush.random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defn seq-zip
  "Returns a zipper for nested sequences, given a root sequence"
  {:added "1.0"}
  [root]
  (zip/zipper seq?
          seq
          (fn [node children] (with-meta children (meta node)))
          root))

(defn ensure-list ;; really make-list-if-not-seq, but close enough for here
  [thing]
  (if (seq? thing)
    thing
    (list thing)))

(defn print-return
  "Prints the provided thing and returns it."
  [thing]
  (println thing)
  thing)

(defn keep-number-reasonable
  "Returns a version of n that obeys limit parameters."
  [n]
  (cond
    (integer? n)
    (cond
      (> n max-number-magnitude) max-number-magnitude
      (< n (- max-number-magnitude)) (- max-number-magnitude)
      :else n)
    :else
    (cond
      (> n max-number-magnitude) (* 1.0 max-number-magnitude)
      (< n (- max-number-magnitude)) (* 1.0 (- max-number-magnitude))
      (and (< n min-number-magnitude) (> n (- min-number-magnitude))) 0.0
      :else n)))

(defn count-points
  "Returns the number of points in tree, where each atom and each pair of parentheses 
   counts as a point."
  [tree]
  (if (seq? tree)
    (inc (apply + (map count-points tree)))
    1))

(defn count-parens
  "Returns the number of paren pairs in tree"
  [tree]
  (if (seq? tree)
    (inc (apply + (map count-parens tree)))
    0))

(defn code-at-point 
  "Returns a subtree of tree indexed by point-index in a depth first traversal."
  [tree point-index]
  (let [index (mod (math/abs point-index) (count-points tree))
        zipper (seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/node z)
        (recur (zip/next z) (dec i))))))

(defn insert-code-at-point
  "Returns a copy of tree with the subtree formerly indexed by
   point-index (in a depth-first traversal) replaced by new-subtree."
  [tree point-index new-subtree]
  (let [index (mod (math/abs point-index) (count-points tree))
        zipper (seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/root (zip/replace z new-subtree))
        (recur (zip/next z) (dec i))))))

(defn remove-code-at-point
  "Returns a copy of tree with the subtree formerly indexed by
   point-index (in a depth-first traversal) removed. The old version would not
   allow removals that result in empty lists, but the current version allows
   this behavior."
  [tree point-index]
  (let [index (mod (math/abs point-index) (count-points tree))
        zipper (seq-zip tree)]
    (if (zero? index)
      tree ;; can't remove entire tree
      (loop [z zipper i index]
        (if (zero? i)
          (zip/root (zip/remove z))
          (if (and (= i 1) ;; zipper can't remove only item from list; instead, replace with empty list
                   (seq? (zip/node z))
                   (= 1 (count (zip/node z))))
            (zip/root (zip/replace z '())) ;; used to just return (zip/root z)
            (recur (zip/next z) (dec i))))))))

; Note: Well, I think I figured out why truncate was there. When I tried running
; the change problem, it threw an exception trying to cast into an int a number
; that was too big. Maybe there's a different principled way to use casting, but 
; 'm just going to add truncate back for now!
(defn truncate
  "Returns a truncated integer version of n."
  [n]
  (if (< n 0)
    (math/round (math/ceil n))
    (math/round (math/floor n))))

(defn walklist
  "Like walk, but only for lists."
  [inner outer form]
  (cond
    (list? form) (outer (apply list (map inner form)))
    (seq? form) (outer (doall (map inner form)))
    :else (outer form)))

(defn postwalklist
  "Like postwalk, but only for lists"
  [f form]
  (walklist (partial postwalklist f) f form))

(defn prewalkseq
  "Like prewalk but only for seqs and uses zippers."
  [f s]
  (loop [z (seq-zip s)] ;; note using modified version of seq-zip for now
    (if (zip/end? z)
      (zip/root z)
      (recur (zip/next (zip/replace z (f (zip/node z))))))))

(defn postwalklist-replace
  "Like postwalk-replace, but only for lists."
  [smap form]
  (postwalklist (fn [x] (if (contains? smap x) (smap x) x)) form))

(defn subst
  "Returns the given list but with all instances of that (at any depth)                                   
   replaced with this. Read as 'subst this for that in list'. "
  [this that lst]
  (postwalklist-replace {that this} lst))

(defn contains-subtree
  "Returns true if tree contains subtree at any level. Inefficient but
   functional implementation."
  [tree subtree]
  (or 
    (= tree subtree)
    (not (= tree (subst (gensym) subtree tree)))))

(defn containing-subtree
  "If tree contains subtree at any level then this returns the smallest
   subtree of tree that contains but is not equal to the first instance of
   subtree. For example, (contining-subtree '(b (c (a)) (d (a))) '(a)) => (c (a)).
   Returns nil if tree does not contain subtree."
  [tree subtree]
  (cond 
    (not (seq? tree)) nil
    (empty? tree) nil
    (some #{subtree} tree) tree
    :else (some (fn [smaller-tree]
                  (containing-subtree smaller-tree subtree))
                tree)))

(defn all-items
  "Returns a list of all of the items in lst, where sublists and atoms all
   count as items. Will contain duplicates if there are duplicates in lst.
   Recursion in implementation could be improved."
  [lst]
  (cons lst (if (seq? lst)
              (apply concat (doall (map all-items lst)))
              ())))

(defn not-lazy
  "Returns lst if it is not a list, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn list-to-open-close-sequence
  [lst]
  (if (seq? lst)
    (flatten (prewalkseq #(if (seq? %) (concat '(:open) % '(:close)) %) lst))
    lst))

;(list-to-open-close-sequence '(1 2 (a b (c) ((d)) e)))

(defn open-close-sequence-to-list
  [sequence]
  (cond (not (seq? sequence)) sequence
        (empty? sequence) ()
        :else (let [opens (count (filter #(= :open %) sequence))
                    closes (count (filter #(= :close %) sequence))]
                (assert (= opens closes)
                        (str "open-close sequence must have equal numbers of :open and :close; this one does not:\n" sequence))
                (let [s (str sequence)
                      l (read-string (string/replace (string/replace s ":open" " ( ") ":close" " ) "))]
                  ;; there'll be an extra ( ) around l, which we keep if the number of read things is >1
                  (if (= (count l) 1)
                    (first l)
                    l)))))

;(open-close-sequence-to-list '(:open 1 2 :open a b :open c :close :open :open d :close :close e :close :close))
;(open-close-sequence-to-list '(:open 1 :close :open 2 :close))
;(open-close-sequence-to-list '(:open :open 1 :close :open 2 :close :close))
             