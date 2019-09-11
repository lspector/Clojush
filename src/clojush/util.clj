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

(defn list-concat
  "Returns a (non-lazy) list of the items that result from calling concat
  on args."
  [& args]
  (apply list (apply concat args)))

(defn not-lazy
  "Returns lst if it is not a seq, or a non-lazy list of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn ensure-list
  "Returns a non-lazy list of the contents of thing if thing is a seq.
  Returns a list containing thing otherwise."
  [thing]
  (if (seq? thing)
    (not-lazy thing)
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
          (recur (list-concat (first remaining) 
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
          (recur (list-concat (first remaining) 
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
              (apply list-concat (doall (map all-items lst)))
              ())))

(defn remove-one
  "Returns sequence s without the first instance of item."
  [item s]
  (let [[without-item with-item] (split-with #(not (= item %)) s)]
    (concat without-item (rest with-item))))

(defn list-to-open-close-sequence
  [lst]
  (if (seq? lst)
    (flatten (prewalkseq #(if (seq? %) (list-concat '(:open) % '(:close)) %) lst))
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
                (let [s (str (not-lazy sequence))
                      l (read-string (string/replace (string/replace s ":open" " ( ") ":close" " ) "))]
                  ;; there'll be an extra ( ) around l, which we remove if the number of things is =1 and that thing is a sequence
                  (if (and (= (count l) 1)
                           (seq? (first l)))
                    (first l)
                    l)))))

;(open-close-sequence-to-list '(:open 1 2 :open a b :open c :close :open :open d :close :close e :close :close))
;(open-close-sequence-to-list '(:open 1 :close :open 2 :close))
;(open-close-sequence-to-list '(:open :open 1 :close :open 2 :close :close))
;(open-close-sequence-to-list '(1 :open 2 3 :close 4))
;(open-close-sequence-to-list '(1))
;(open-close-sequence-to-list '(:close 5 :open))
;(open-close-sequence-to-list (list-to-open-close-sequence '(5)))

(defn test-and-train-data-from-domains
  "Takes a list of domains and creates a set of (random) train inputs and a set of test
   inputs based on the domains. Returns [train test]. A program should not
   be considered a solution unless it is perfect on both the train and test
   cases."
  [domains]
  (vec
    (apply 
      mapv 
      concat 
      (map (fn [[input-set n-train n-test]]
             (if (fn? input-set)
               (vector (repeatedly n-train input-set)
                       (repeatedly n-test input-set))
               (let [shuffled-inputs (shuffle input-set)
                     train-inputs (if (= n-train (count input-set))
                                    input-set ; NOTE: input-set is not shuffled if the same size as n-train
                                    (take n-train shuffled-inputs))
                     test-inputs (if (= n-test (count input-set))
                                   input-set ; NOTE: input-set is not shuffled if the same size as n-test
                                   (drop n-train shuffled-inputs))]
                 (assert (= (+ n-train n-test) (count input-set)) 
                         "Sizes of train and test sets don't add up to the size of the input set.")
                 (vector train-inputs test-inputs))))
           domains))))

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

(defn sequence-similarity
  [sequence1 sequence2]
  "Returns a number between 0 and 1, indicating how similar the sequences are as a normalized,
  inverted Levenshtein distance, with 1 indicating identity and 0 indicating no similarity."
  (if (and (empty? sequence1) (empty? sequence2))
    1
    (let [dist (levenshtein-distance sequence1 sequence2)
          max-dist (max (count sequence1) (count sequence2))]
      (/ (- max-dist dist) max-dist))))

;;;;;;;
;; Hamming Distance
(defn hamming-distance
  "Calculates the Hamming distance between two sequences, including strings"
  [seq1 seq2]
  (apply + (map #(if (= %1 %2) 0 1)
                  seq1 seq2)))

;;;;;;;;;;;;;;:::::;;;;;;;;;;;;;;
;; Simple Statistic Functions
;; From: https://github.com/clojure-cookbook/clojure-cookbook/blob/master/01_primitive-data/1-20_simple-statistics.asciidoc

(defn mean
  [coll]
  "https://github.com/clojure-cookbook/clojure-cookbook/blob/master/01_primitive-data/1-20_simple-statistics.asciidoc"
  (let [sum (apply +' coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn average
  [& args]
  (mean args))

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


(defn nested-parens
 [pattern prog]
 (let [contents (re-seq pattern prog)
       coll (concat (map first contents))
       coll1 (if (nil? contents)
               nil
               (reduce concat (for [content contents]
                                (nested-parens pattern (subs (first content) 1 (- (.length (first content)) 1))))))
       coll (concat coll coll1)]
   coll))


(defn tagspace-initialization
  [prog num-tags state]
  (let [single-instrs (string/split (string/replace prog #"[\(\)]" "") #" ")
        list-instrs (nested-parens #"(?=\()(?:(?=.*?\((?!.*?\1)(.*\)(?!.*\2).*))(?=.*?\)(?!.*?\2)(.*)).)+?.*?(?=\1)[^(]*(?=\2$)" prog)
        contents (concat single-instrs list-instrs)]
    (assoc state :tag (reduce merge (for [pair (zipmap (shuffle contents) (range 0 num-tags (max 1 (quot num-tags (count contents)))))]
                                      (assoc (or (:tag state) (sorted-map))
                                                      (second pair)
                                                      (let [code (first pair)]
                                                        (if (re-find #"return_" code)
                                                          (str "environment_begin " code " environment_end")
                                                          code))))))))

;; Helper functions for hertiable tagspace intiliazation

(def pattern #"(?=\()(?:(?=.*?\((?!.*?\1)(.*\)(?!.*\2).*))(?=.*?\)(?!.*?\2)(.*)).)+?.*?(?=\1)[^(]*(?=\2$)")

(defn preprocess_tagged_code
  [fragment]
  (if (count (rest (nested-parens pattern fragment)))
  (let [fragment (string/replace (subs fragment 1)  #" \(\d+" "")
  fragment (string/replace fragment  #" \)" "")]
  (str "(" fragment " )"))
))

(defn re-pos [re s]
        (loop [m (re-matcher re s)
               res {}]
          (if (.find m)
            (recur m (assoc res (.start m) (.group m)))
            res)))

(defn make-balanced
  [expr]
    (let [re-poses (re-pos #"[\(\)]" expr)
    stack (atom ())]
      (for [index (sort (keys re-poses))]
        (cond
          (= (get re-poses index) "(") (swap! stack conj index)
          (= (get re-poses index) ")") (if (not (empty? @stack))(swap! stack pop))
        :else nil
))))

(defn preprocess-tagspace ;; to handle the case when there are more begin_tag instructions than end_tag instructons.
  [expr]
  (let [indices (last (make-balanced expr))
    exprr (string/split expr #"")]
    (string/join "" (keep-indexed #(if (not (some #{%1} indices)) %2) exprr))
))


(defn tagspace-initialization-heritable
  [prog state]
  (let [prog (string/replace prog #"[\(]" "<")
    prog (string/replace prog #"[\)]" ">")
    prog (string/replace prog #"begin_tag_" "(")
    prog (string/replace prog #"tag_end" ")")
    list-instrs (nested-parens pattern (preprocess-tagspace prog))]
    (assoc state :tag (reduce merge (for [fragment list-instrs]
      (let [fragment (preprocess_tagged_code fragment)
        fragments (string/split fragment #" ")]
        (assoc (or (:tag state) (sorted-map))
          (Integer/parseInt (re-find #"\d+" fragment))
          (let [code (str "(" (string/join " " (drop-last (drop 1 fragments))) ")")]
            (if (re-find #"return_" code)
              (str "environment_begin " code " environment_end")
              code)))))))))

(defn closest-tag
  "Returns the closest match to the given tag
   in the given list."
  [tag tag-list]
  (loop [associations tag-list]
    (if (or (empty? (rest associations))
            (<= tag (first associations)))
      (first associations)
      (recur (rest associations)))))

(defn intial-tagspace-utilization
 [prog state]
 (let [init-tags (keys (get state :tag))
       prog-tagspace (re-seq #"\w*tag[ged]*_\w+" prog)
       curr-tags init-tags
       result (for [instr prog-tagspace]
                (let [tag (Integer. (re-find #"\d+" instr))]
                  (if (= "tag_" (subs instr 0 4))
                    (if (some #(= tag %) init-tags) 
                      (remove #{tag} init-tags))
                    (conj curr-tags tag))
                  (if (= "untag_" (subs instr 0 6))
                    (remove #{tag} curr-tags))
                  (if (= "tagged_" (subs instr 0 7))
                    (if (some #(= (closest-tag tag curr-tags) %) init-tags)
                      tag))))]
   (list (count (filter identity result)) (count init-tags))))



(defn stacks-depth
  [state]
  (let [new-state (dissoc state :tag :return :environment:genome)]
       (for [key (keys new-state)]
            (count (get new-state key)))))



; The functions to calculate modularity metrics
(defn exp [x n]
     (if (zero? n) 1
         (*' x (exp x (dec n)))))

(defn parenthetic-contents
  "Generate parenthesized contents in string as pairs (level, contents)."
  [expr]
  (let [result (atom ())
        stack (atom ())
        lookout-start (atom -1)
        lookout-end (atom -1)
        part-of-string? (atom false)]
    (do
      (doseq [index (range (count expr))]
        (cond
          (and (= (get expr index) \() (not @part-of-string?)) (do
                                                                 (swap! stack conj index)
                                                                 (reset! lookout-end index)
                                                                 (if (and (> @lookout-start 0) (> @lookout-end @lookout-start) (> (count (string/split (subs expr (+ @lookout-start 1) @lookout-end) #" ")) 0))
                                                                   (doseq [x (string/split (subs expr (+ @lookout-start 1) @lookout-end) #" ")]
                                                                     (swap! result conj (list 1 x)))))
          (= (get expr index) \") (if @part-of-string?
                                    (reset! part-of-string? false)
                                    (reset! part-of-string? true))

          (and (= (get expr index) \)) (not (empty? @stack)) (not @part-of-string?))
          (let [start (first @stack)
                _ (swap! stack pop)]
            (do
              (if (= (count @stack) 1)
                (reset! lookout-start index))
              (swap! result conj (list (count @stack) (subs expr (inc start) index)))))
          ;:else nil
          ))
      (prn expr)
      (doseq [x (string/split (subs (string/join  (drop-last (subs expr 1))) (+ @lookout-start 1)) #" ")]
        (swap! result conj (list 1 x)))
      ;(prn result)
      (def final (map (fn [x] (nth x 1)) (filter (fn [x] (and (= (nth x 0) 1) (not= (nth x 1) ""))) (reverse @result))))
      (map str (read-string (str "(" (string/join " " final) ")"))))))

(defn all-continuous-seqs
  [expr]
  (let [seqs (atom ())
        dups (for [[id freq] (frequencies expr)  ;; get the frequencies, destructure
                   :when (> freq 1)]            ;; this is the filter condition
               id) ]
    (do
      (doseq [dup dups]
        (swap! seqs conj (vector dup)))
      (let [expr (distinct expr)]
        (doseq [i (range 1 (inc (count expr)))
                j (range (inc (- (count expr) i)))]
          (swap! seqs conj (subvec (vec expr) j (+ j i))))
      )
      (sort @seqs)
    @seqs)))

(defn GetRidOfNonOriginalInstrs1
  "Gets rid of non-original instrcutions and empty sequences."
  [exec meta-tr maxid]
  (let [result-exec (atom ())
        result-meta (atom ())]
    (doseq [[x y] (map list exec meta-tr)]
      (cond
        (= x '()) nil
        (seq? x) (let [res (GetRidOfNonOriginalInstrs1 x y maxid)]
                   (if-not (empty? (first res))
                     (do
                       (swap! result-exec conj (first res))
                       (swap! result-meta conj (last res)))))

        (> y maxid) nil
        :else (do
                (swap! result-exec conj x)
                (swap! result-meta conj y))))
    (list (reverse @result-exec) (reverse @result-meta))))



(defn GetRidOfNonOriginalInstrs
  "Gets rid of non-original instrcutions and empty sequences."
  [meta-tr maxid]
  (let [result-meta (atom ())]
    (doseq [x meta-tr]
      (cond
        (= x '()) nil
        (seq? x) (let [res (GetRidOfNonOriginalInstrs x maxid)]
                   (if-not (empty? res)
                     (do
                       (swap! result-meta conj res))))
        (> x maxid) nil
        :else (do
                (swap! result-meta conj x))))
    (reverse @result-meta)))

(defn local-mods
  "Given a sequence of instructions, output a list of all possible modules. For instance given [(2 2),(3 3),(4 4)], output the modules [(2 2)], [(3 3)], [(4 4)], [(2 2),(3 3)], [(2 2),(3 3)], [(2 2),(3 3),(4 4)]"
  [temp]
  (let  [temp (sort-by first temp)
         local-modules (all-continuous-seqs (vec temp))]
    (remove #(= % (vec temp)) local-modules)))


(defn mod-metrics
  [exec-trace meta-trace]
  (let [exec-trace1 (reverse exec-trace)
        ;_ (prn exec-trace)
        meta-trace1 (reverse meta-trace)
        ;_ (prn meta-trace1)
        ;; Since the first element of meta-trace is the list of identifiers of the instruction in the original program, maxid can be calculated as the max of those identifers
        meta-trace (GetRidOfNonOriginalInstrs meta-trace1 (if-not (empty? (flatten (first meta-trace1)))
                                                            (apply max (flatten (first meta-trace1)))
                                                            0))
        ;; (1 (2 3) (4 (5 6) 7)) will be converted to ([1 1] [2 3] [4 7])  
        metadata-trace (map #(if (seq? %)  (let [flt (flatten %)] (vector (first flt) (last flt)))  (vector % %)) meta-trace)
        ;; total number of instructions with distinct identifiers 
        vocab-length (count (distinct (filter (fn [x] (= (first x) (last x))) metadata-trace)))
        len (count metadata-trace)
        seqs (atom ())
        
        ;filter out the moduels appearing only once
        m-trace (let [freq (frequencies metadata-trace)]
                  (atom (filter #(not= (get freq %) 1) metadata-trace)))
        reuse (atom 0)
        repetition (atom 0)
        i (atom 0)]
    (while (< @i (count @m-trace))
      (do
        (let [temp (atom ())]
          (do
            (reset! m-trace
                    (reverse (reduce #(let [lst (first %1)
                                            m (nth lst 0)
                                            n (nth lst 1)]
                                        (if (and (not (empty? lst)) (= (- n m) @i) (>= (nth %2 0) m) (<= (nth %2 1) n) (not= lst %2))
                                          (let [_ (swap! temp conj %2)
                                                ;_ (prn @temp)
                                                ]
                                            %1)
                                          (let [_ (if-not (empty? @temp) (swap! seqs concat (local-mods @temp)))
                                                _  (reset! temp '())]
                                            (conj %1 %2)))) '() @m-trace)))
            (let [_ (if-not (empty? @temp) (swap! seqs concat (local-mods @temp)))
                  _  (reset! temp '())])
            )
          )
        ;(prn @m-trace)
        (swap! i inc)))
    (swap! seqs concat (remove #(= % (vec @m-trace)) (all-continuous-seqs (vec @m-trace))))
    (let [freq-temp (frequencies @seqs)]
      (doseq [elem freq-temp]
        (swap! reuse +' (*' (inc (- (nth (last (first elem)) 1) (nth (first (first elem)) 0))) (math/expt 2 (last elem))))))
    (reset! reuse (unchecked-float (/ @reuse (exp 2 vocab-length))))

    (list @reuse 0)
  ))




;; the following implementation is old but correct. 
(defn mod-metrics1
  [exec-trace meta-trace]
  (let [;; the lists obtained from program execution are in reverse order
        exec-trace1 (reverse exec-trace)
        meta-trace1 (reverse meta-trace)
        ;; Since the first element of meta-trace is the list of identifiers of the instruction in the original program, maxid can be calculated as the max of those identifers
        combined (GetRidOfNonOriginalInstrs exec-trace1 meta-trace1 (if-not (empty? (flatten (first meta-trace1)))
                                                                      (apply max (flatten (first meta-trace1)))
                                                                      0))
        exec-trace (first combined)
        meta-trace (last combined)
        ;;(ins1 (inst2 inst3))  will be converted to ('inst1' 'inst2 inst3')
        actual-exec-trace (doall (map (fn [x] (let [x-str (str x)]
                                                (if (seq? x)
                                                  (subs x-str 1 (dec (count x-str)))
                                                  x-str)))
                                      exec-trace))
        ;; (1 (2 3) (4 (5 6) 7)) will be converted to ([1 1] [2 3] [4 7])  
        metadata-trace (map (fn [x] (if (seq? x)
                                      (loop [f (first x) 
                                             l (last x)]
                                        (if (not (or (seq? f) (seq? l)))
                                          (vector f l)
                                          (recur (if (seq? f)
                                                   (first f)
                                                   f)
                                                 (if (seq? l)
                                                   (last l)
                                                   l))))
                                      (vector x x))) meta-trace)
        ;; total number of instructions with distinct identifiers 
        vocab-length (count (distinct (filter (fn [x] (= (first x) (last x))) metadata-trace))) 
        len (count metadata-trace)
        seqs (atom ())
        m-trace (atom metadata-trace)
        reuse (atom 0)
        repetition (atom 0)
        i (atom 1)]
    (while (<= @i (count @m-trace))
      (let [j (atom 0)]
        (while (< @j (count @m-trace))
          (do
            (let [m (first (nth @m-trace @j))
                  n (last (nth @m-trace @j))
                  temp (atom ())]
              (if (= (- n m) @i)
                (do
                  (doseq [k (range (inc @i))]
                    (if (and (< @j (dec (count @m-trace))) (>= (nth (nth @m-trace (inc @j)) 0) m) (<= (nth (nth @m-trace (inc @j)) 1) n) (not= (nth @m-trace (inc @j)) (nth @m-trace @j)))
                      (let [f (nth @m-trace (inc @j))]
                        (reset! m-trace (doall (keep-indexed #(if (not= %1 (inc @j)) %2) @m-trace)))
                        (swap! temp conj f)
                        )
                      )
                    )
                  (let [_ (reset! temp (sort-by first @temp))
                        local-modules (all-continuous-seqs (vec @temp))
                        local-modules (remove #(= % (vec @temp)) local-modules)]
                    (swap! seqs concat local-modules)
                    )
                  )
                ))
            (swap! j inc)
            )
          )
        (swap! i inc)
        ))
    (swap! seqs concat (remove #(= % (vec @m-trace)) (all-continuous-seqs (vec @m-trace))))

    (let [freq-temp (frequencies @seqs)
          freq-temp (remove #(= (last %) 1) freq-temp)]

      (doseq [elem freq-temp]        
        (swap! reuse +' (*' (inc (- (nth (last (first elem)) 1) (nth (first (first elem)) 0))) (math/expt 2 (last elem)) ))
        )
      )
    (reset! reuse (unchecked-float (/ @reuse (math/expt 2 vocab-length))))

    (let [mapping (zipmap metadata-trace actual-exec-trace)
          _ (swap! seqs distinct)
          modules-temp (map (fn [x] (vals (select-keys mapping x))) @seqs)
          modules-temp (map (fn [x] (string/join " " x)) modules-temp)
          modules-temp (map (fn [x] (string/replace x #"[()]" "")) modules-temp)
          freq-temp (frequencies modules-temp)
          unique-instrs (count (distinct (apply concat (for [elem freq-temp]        
                                                         (string/split (first elem) #" ")))))
          freq-temp (remove #(= (last %) 1) freq-temp)
          ]
  
      (reset! repetition (reduce +' (for [elem freq-temp]        
                                     (*' (count (string/split (first elem) #" ")) (math/expt 2 (last elem))))))
      
      (reset! repetition (unchecked-float (/ @repetition (math/expt 2 unique-instrs))))
    
      )

    (list @reuse @repetition)
    )
  )
