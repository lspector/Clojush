(ns clojush.mod_metrics
  (:use clojush.globals)
  (:require [clojure.math.numeric-tower :as math]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The functions to calculate the modularity metrics;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn attach-ids
  [orig ids]
  (loop [x 0
         result '()]
    (if (= x (count orig))
      (reverse result)
      (recur (inc x) (let [elem (nth orig x)
                           elem-id (nth ids x)]
                       (if (seq? elem)
                         (conj result (attach-ids elem elem-id))
                         (conj result (hash-map :instr elem, :id elem-id))))))))

(defn remove-ids
  "The second argument decides whether you want exec stack (0) or the identifiers (1)."
  [comb y]
  (loop [x 0
         result '()]
    (if (= x (count comb))
      (reverse result)
      (recur (inc x) (let [elem (nth comb x)]
                       (if (seq? elem)
                         (conj result (remove-ids elem y))
                         (conj result (get elem y))))))))

(defn correct-ids
  "Sometimes the instructions can call some other instruction that has not been assigned an identifier.
  This function assigns an id to such instructions. For example, exec_do*while calls exec_while.
  'highid' ia an atom containing a high number"
  [comb highid]
  (doall (map (fn [x] (if (and (map? x) (= (set (keys x)) #{:id :instr}))
                        x
                        (if (seq? x)
                          (correct-ids x (let [_ (swap! highid inc)]
                                           highid))
                          (hash-map :instr x, :id (vector @highid -1)))))
              comb)))


(defn assign-ids
  "Assign identifiers (starts from 0) to the instruction in the expression. Retains the braces. Just replaces the instructions
   with their identifiers."
  [expr init-id init-dep]
  (loop [x 0 ;index
         y init-id ;id
         result '()
         dep init-dep ;level of parenthesis
         ]
    (if (= x (count expr))
      (reverse result)
      (recur (inc x) (let [_ (swap! y inc)]
                       y)
             (let [elem (nth expr x)]
               (if (seq? elem)
                 (conj result (assign-ids elem (let [_ (swap! y dec)]
                                                 y) (inc dep)))
                 (conj result (vector @y dep)))) dep))))


(defn all-continuous-seqs
  [expr]
  (let [expr (map #(vector (first %) (second %)) expr)
        seqs (atom ())
        freq (frequencies expr)] ;; get the frequencies
    (do
      (doseq [[x y] freq]
        (doseq [k (range (dec y))] (swap! seqs conj (vector x))))

      (let [expr (distinct expr)]
        (doseq [i (range 1 (inc (count expr)))
                j (range (inc (- (count expr) i)))]
          (let [module (subvec (vec expr) j (+ j i))]
            (if (not= (reduce #(if (and (not= %1 -1) (= (first %2) (inc (last %1))))  (vector (first %1) (last %2)) -1) module) -1)
              (swap! seqs conj module)))))
      @seqs)))

(defn GetRidOfNonOriginalInstrs
  "Gets rid of non-original instructions (instructions which were not present in the original program) and empty sequences."
  [meta-tr maxid]
  (remove #(or (empty? %) (> (first %) maxid) (> (second %) maxid)) meta-tr))

(defn local-mods
  "Given a sequence of instructions, output a list of all possible modules.
  For instance given [(2 2),(3 3),(4 4)], output the modules [(2 2)], [(3 3)], [(4 4)], [(2 2),(3 3)], [(2 2),(3 3)], [(2 2),(3 3),(4 4)]"
  [big-module temp]
  (let  [big-module (vector (first big-module) (second big-module))
         temp (map #(vector (first %) (second %)) temp)
         temp (sort-by first temp)
         local-modules (all-continuous-seqs (vec temp))]
    (if (= (vector (first (first (first local-modules))) (first (last (first local-modules)))) big-module)
      (rest local-modules)
      local-modules)))

(defn mod-metrics
  "Returns the values of reuse and repetition metrics for a given execution trace. Inputs are: raw trace, corresponding identifiers.
  Use the reuse() function if only reuse is required as this function runs very slow."
  [exec-trace meta-trace]
  (let [exec-trace1 (reverse exec-trace)
        meta-trace1 (reverse meta-trace)

        metadata-trace (map #(if (seq? %) (let [flt (flatten %)] (if (or (empty? flt) (= (count flt) 2)) ; second test is for removing duplicates: ([47 1]) [47 1]
                                                                   []
                                                                   (vector (first flt) (last (butlast flt)) (apply min (take-nth 2 (rest flt))))))
                                          (vector (first %) (first %) (last %)))
                            meta-trace1)
        combined (zipmap (map #(butlast %) metadata-trace) exec-trace1)
        metadata-trace (GetRidOfNonOriginalInstrs metadata-trace (if-not (empty? (flatten (first meta-trace1)))
                                                                   (apply max (flatten (first meta-trace1)))
                                                                   0))

        ;; total number of instructions with distinct identifiers
        vocab-length (count (distinct (filter (fn [x] (= (first x) (second x))) (map #(vector (first %) (second %)) metadata-trace))))
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
                  n (second (nth @m-trace @j))
                  l (last (nth @m-trace @j))
                  temp (atom ())]
              (if (= (- n m) @i)
                (do
                  (while (and (< @j (dec (count @m-trace))) (>= (nth (nth @m-trace (inc @j)) 0) m) (<= (nth (nth @m-trace (inc @j)) 1) n)
                              (>= (nth (nth @m-trace (inc @j)) 2) l) (not= (nth @m-trace (inc @j)) (nth @m-trace @j)))
                    (let [f (nth @m-trace (inc @j))]
                      (reset! m-trace (doall (keep-indexed #(if (not= %1 (inc @j)) %2) @m-trace)))
                      (swap! temp conj f)))
                  (swap! seqs concat (local-mods (nth @m-trace @j) @temp)))))
            (swap! j inc)))
        (swap! i inc)))

    (swap! seqs concat (all-continuous-seqs (sort-by first (vec @m-trace))))

    (let [freq-temp (frequencies @seqs)
          freq-temp (remove #(= (last %) 1) freq-temp)]
      (doseq [elem freq-temp]
        (swap! reuse +' (*' (inc (- (nth (last (first elem)) 1) (nth (first (first elem)) 0))) (math/expt 2 (last elem))))))
    (reset! reuse (unchecked-float (/ @reuse (math/expt 2 vocab-length))))


    (let [_ (swap! seqs distinct)
          modules-temp (map (fn [x] (vals (select-keys combined x))) @seqs)
          modules-temp (map (fn [x] (string/join " " x)) modules-temp)
          freq-temp (frequencies modules-temp)
          unique-instrs (count (distinct (string/split (string/replace (str exec-trace1) #"[()]" "") #" ")))
          freq-temp (remove #(= (last %) 1) freq-temp)]

      (reset! repetition (reduce +' (for [elem freq-temp]
                                      (*' (count (string/split (first elem) #" ")) (math/expt 2 (last elem))))))

      (reset! repetition (unchecked-float (/ @repetition (math/expt 2 unique-instrs)))))

    (list @reuse @repetition)))




(defn reuse
  "Returns the values of reuse metric for a given execution trace. Inputs are: raw trace, corresponding identifiers. Faster than mod-metrics function."
  [exec-trace meta-trace]
  (let [exec-trace1 (reverse exec-trace)
        meta-trace1 (reverse meta-trace)

        metadata-trace (map #(if (seq? %) (let [flt (flatten %)] (if (or (empty? flt) (= (count flt) 2)) ; second test is for removing duplicates: ([47 1]) [47 1]
                                                                   []
                                                                   (vector (first flt) (last (butlast flt)) (apply min (take-nth 2 (rest flt))))))
                                          (vector (first %) (first %) (last %)))
                            meta-trace1)
        metadata-trace (GetRidOfNonOriginalInstrs metadata-trace (if-not (empty? (flatten (first meta-trace1)))
                                                                   (apply max (flatten (first meta-trace1)))
                                                                   0))

        ;; total number of instructions with distinct identifiers
        vocab-length (count (distinct (filter (fn [x] (= (first x) (second x))) (map #(vector (first %) (second %)) metadata-trace))))
        seqs (atom ())

        ;filter out the modules appearing only once
        m-trace (let [freq (frequencies metadata-trace)]
                  (atom (filter #(not= (get freq %) 1) metadata-trace)))

        reuse (atom 0)
        i (atom 1)]
    (while (<= @i (count @m-trace))
      (let [j (atom 0)]
        (while (< @j (count @m-trace))
          (do
            (let [m (first (nth @m-trace @j))
                  n (second (nth @m-trace @j))
                  l (last (nth @m-trace @j))
                  temp (atom ())]
              (if (= (- n m) @i)
                (do
                  (while (and (< @j (dec (count @m-trace))) (>= (nth (nth @m-trace (inc @j)) 0) m) (<= (nth (nth @m-trace (inc @j)) 1) n)
                              (>= (nth (nth @m-trace (inc @j)) 2) l) (not= (nth @m-trace (inc @j)) (nth @m-trace @j)))
                    (let [f (nth @m-trace (inc @j))]
                      (reset! m-trace (doall (keep-indexed #(if (not= %1 (inc @j)) %2) @m-trace)))
                      (swap! temp conj f)))
                  (swap! seqs concat (local-mods (nth @m-trace @j) @temp)))))
            (swap! j inc)))
        (swap! i inc)))

    (swap! seqs concat (all-continuous-seqs (sort-by first (vec @m-trace))))

    (let [freq-temp (frequencies @seqs)
          freq-temp (remove #(= (last %) 1) freq-temp)]
      (doseq [elem freq-temp]
        (swap! reuse +' (*' (inc (- (nth (last (first elem)) 1) (nth (first (first elem)) 0))) (math/expt 2 (last elem))))))
    (reset! reuse (unchecked-float (/ @reuse (math/expt 2 vocab-length))))

    ; By default also adds a zero value of repetition
    (list @reuse 0.0)))