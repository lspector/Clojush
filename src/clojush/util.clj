(ns clojush.util
  (:use clojush.globals)
  (:require [clojure.math.numeric-tower :as math]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(def literals
  (atom
    {:integer integer?
     :float float?
     :char char?
     :string string?
     :boolean (fn [thing] (or (= thing true) (= thing false)))
     :vector_integer (fn [thing] (and (vector? thing) (integer? (first thing))))
     :vector_float (fn [thing] (and (vector? thing) (float? (first thing))))
     :vector_string (fn [thing] (and (vector? thing) (string? (first thing))))
     :vector_boolean (fn [thing] (and (vector? thing) (or (= (first thing) true) (= (first thing) false))))}))
     

(defn recognize-literal
  "If thing is a literal, return its type -- otherwise return false."
  [thing]
  (loop [m (seq @literals)]
    (if-let [[type pred] (first m)]
      (if (pred thing) type
        (recur (rest m)))
      nil)))

;; Add new literals by just assoc'ing on the new predicate. e.g.:
;; (swap! literals :symbol symbol?)

(def debug-recent-instructions ())

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

(defn round-to-n-decimal-places
  "If a number, rounds float f to n decimal places."
  [f n]
  (if (not (number? f))
    f
    (let [factor (math/expt 10 n)]
      (double (/ (math/round (* f factor)) factor)))))

(defn count-parens
  "Returns the number of paren pairs in tree"
  [tree]
  (loop [remaining tree
         total 0]
    (cond (not (seq? remaining)) 
          total
          ;; 
          (empty? remaining) 
          (inc total)
          ;;
          (not (seq? (first remaining))) 
          (recur (rest remaining) 
                 total)
          ;;
          :else 
          (recur (concat (first remaining) 
                         (rest remaining)) 
                 (inc total)))))

(defn count-points
  "Returns the number of points in tree, where each atom and each pair of parentheses 
   counts as a point."
  [tree]
  (loop [remaining tree
         total 0]
    (cond (not (seq? remaining)) 
          (inc total)
          ;; 
          (empty? remaining) 
          (inc total)
          ;;
          (not (seq? (first remaining))) 
          (recur (rest remaining) 
                 (inc total))
          ;;
          :else 
          (recur (concat (first remaining) 
                         (rest remaining)) 
                 (inc total)))))

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

; Note: Well, I (Tom) think I figured out why truncate was there. When I tried running
; the change problem, it threw an exception trying to cast into an int a number
; that was too big. Maybe there's a different principled way to use casting, but 
; I'm just going to add truncate back for now!
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

(defn test-and-train-data-from-domains
  "Takes a list of domains and creates a set of (random) train inputs and a set of test
   inputs based on the domains. Returns [train test]. A program should not
   be considered a solution unless it is perfect on both the train and test
   cases."
  [domains]
  (apply mapv concat (map (fn [[input-set n-train n-test]]
                            (if (fn? input-set)
                              (vector (repeatedly n-train input-set)
                                      (repeatedly n-test input-set))
                              (let [shuffled-inputs (shuffle input-set)
                                    train-inputs (if (= n-train (count input-set))
                                                   input-set ; NOTE: input-set is not shuffled if it is the same size as n-train
                                                   (take n-train shuffled-inputs))
                                    test-inputs (if (= n-test (count input-set))
                                                   input-set ; NOTE: input-set is not shuffled if it is the same size as n-test
                                                   (drop n-train shuffled-inputs))]
                                (assert (= (+ n-train n-test) (count input-set)) "Sizes of train and test sets don't add up to the size of the input set.")
                                (vector train-inputs test-inputs))))
                          domains)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from https://github.com/KushalP/mailcheck-clj/blob/master/src/mailcheck/levenshtein.clj

(defn compute-next-row
  "computes the next row using the prev-row current-element and the other seq"
  [prev-row current-element other-seq pred]
  (reduce
    (fn [row [diagonal above other-element]]
      (let [update-val (if (pred other-element current-element)
                         ;; if the elements are deemed equivalent according to the predicate
                         ;; pred, then no change has taken place to the string, so we are
                         ;; going to set it the same value as diagonal (which is the previous edit-distance)
                         diagonal
                         ;; in the case where the elements are not considered equivalent, then we are going
                         ;; to figure out if its a substitution (then there is a change of 1 from the previous
                         ;; edit distance) thus the value is diagonal + 1 or if its a deletion, then the value
                         ;; is present in the columns, but not in the rows, the edit distance is the edit-distance
                         ;; of last of row + 1 (since we will be using vectors, peek is more efficient)
                         ;; or it could be a case of insertion, then the value is above+1, and we chose
                         ;; the minimum of the three
                         (inc (min diagonal above (peek row))))]
                         
        (conj row update-val)))
    ;; we need to initialize the reduce function with the value of a row, since we are
    ;; constructing this row from the previous one, the row is a vector of 1 element which
    ;; consists of 1 + the first element in the previous row (edit distance between the prefix so far
    ;; and an empty string)
    [(inc (first prev-row))]
    ;; for the reduction to go over, we need to provide it with three values, the diagonal
    ;; which is the same as prev-row because it starts from 0, the above, which is the next element
    ;; from the list and finally the element from the other sequence itself.
    (map vector prev-row (next prev-row) other-seq)))

(defn levenshtein-distance
  "Levenshtein Distance - http://en.wikipedia.org/wiki/Levenshtein_distance
     In information theory and computer science, the Levenshtein distance is a
     metric for measuring the amount of difference between two sequences. This
     is a functional implementation of the levenshtein edit
     distance with as little mutability as possible.
     Still maintains the O(n*m) guarantee."
  [a b & {p :predicate  :or {p =}}]
  (cond
    (empty? a) (count b)
    (empty? b) (count a)
    :else (peek
            (reduce
              ;; we use a simple reduction to convert the previous row into the next-row  using the
              ;; compute-next-row which takes a current element, the previous-row computed so far
              ;; and the predicate to compare for equality.
              (fn [prev-row current-element]
                (compute-next-row prev-row current-element b p))
              ;; we need to initialize the prev-row with the edit distance between the various prefixes of
              ;; b and the empty string.
              (range (inc (count b)))
              a))))

;;;;;;;;;;;;;;:::::;;;;;;;;;;;;;;
;; Simple Statistic Functions
;; From: https://github.com/clojure-cookbook/clojure-cookbook/blob/master/01_primitive-data/1-20_simple-statistics.asciidoc

(defn mean
  [coll]
  "https://github.com/clojure-cookbook/clojure-cookbook/blob/master/01_primitive-data/1-20_simple-statistics.asciidoc"
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median
  [coll]
  "https://github.com/clojure-cookbook/clojure-cookbook/blob/master/01_primitive-data/1-20_simple-statistics.asciidoc"
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
           (mean [bottom-val top-val])))))
