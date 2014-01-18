(ns clojush.pushgp.genetic-operators
  (:use [clojush.util]
        [clojush.random]
        [clojush.globals]
        [clojush.individual]
        [clojush.node-selection])
  (:require [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genetic operators

(defn mutate 
  "Returns a mutated version of the given individual."
  [ind {:keys [mutation-max-points max-points atom-generators maintain-ancestors]
        :as argmap}]
  (let [new-program (insert-code-at-point (:program ind) 
                                          (select-node-index (:program ind) argmap)
                                          (random-code mutation-max-points atom-generators))]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))

(defn crossover 
  "Returns a copy of parent1 with a random subprogram replaced with a random 
   subprogram of parent2."
  [parent1 parent2 {:keys [max-points maintain-ancestors]
                    :as argmap}]
  (let [new-program (insert-code-at-point 
                      (:program parent1) 
                      (select-node-index (:program parent1) argmap)
                      (code-at-point (:program parent2)
                                     (select-node-index (:program parent2) argmap)))]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:program parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

(defn boolean-gsxover 
  "Returns a child produced from parent1 and parent2 using boolean geometric
   semantic crossover. The child will be of the form:
   (new-random-code exec_if parent1-code parent2-code)."
  [parent1 parent2 {:keys [boolean-gsxover-new-code-max-points max-points atom-generators maintain-ancestors]}]
  (let [new-program (list (random-code boolean-gsxover-new-code-max-points atom-generators) 
                          'exec_if 
                          (:program parent1) 
                          (:program parent2))]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:program parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

(defn delete-mutate
  "Returns the individual with between 1 and 4 points deleted. The points can be
   single instructions or parenthetical pairs. The number of points is based
   roughly on a binomial distribution with n=4 and p=0.25, moved up one so that
   0 is never chosen. This results in the following probabilities for numbers
   of deletions:
   p(1) = 0.32
   p(2) = 0.42
   p(3) = 0.21
   p(4) = 0.05"
  [ind {:keys [maintain-ancestors]}]
  (let [new-program (loop [prog (:program ind)
                           how-many (let [prob (lrand)]
                                      (cond
                                        (< prob 0.32) 1
                                        (< prob 0.74) 2
                                        (< prob 0.95) 3
                                        true 4))]
                      (if (zero? how-many)
                        prog
                        (recur (let [point-index (lrand-int (count-points prog))
                                     point (code-at-point prog point-index)]
                                 (if (and (seq? point)
                                          (< (lrand) 0.2))
                                   (remove-parens-at-point prog point-index)
                                   (remove-code-at-point prog point-index)))
                               (dec how-many))))]
    (make-individual :program new-program :history (:history ind)
                     :ancestors (if maintain-ancestors
                                  (cons (:program ind) (:ancestors ind))
                                  (:ancestors ind)))))

(defn add-parentheses-mutate 
  "Returns a version of the given individual with one pair of parentheses added
   somewhere. Not compatible with (currently 'experimental') tagged code macros."
  [ind {:keys [max-points maintain-ancestors]}]
  (let [expression (:program ind) 
        new-program (let [expstr (str expression)
                          chars (count expstr)
                          space-indices (filter #(= (nth expstr %) \space) (range chars))]
                      (if (< (count space-indices) 2)
                        expression ;; can't add parentheses if less than two spaces
                        (let [i (lrand-nth space-indices)
                              j (lrand-nth (remove #(= % i) space-indices))
                              start (min i j)
                              stop (max i j)]
                          (read-string (str (subs expstr 0 start)
                                            " ( "
                                            (subs expstr (inc start) stop)
                                            " ) "
                                            (subs expstr (inc stop)))))))]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))

(defn tagging-mutate
  "Returns a version of the given individual with a piece of code replaced by a tag
   reference, and with an expression that tags the replaced code with the same tag added
   to the beginning of the individual's program."
  [ind tag-limit {:keys [max-points maintain-ancestors]
                  :as argmap}]
  (let [old-program (:program ind)
        index-to-tag (select-node-index old-program argmap)
        tag (rand-int tag-limit)
        tagging-instruction (symbol (str "tag_exec_" (str tag)))
        tag-ref-instruction (symbol (str "tagged_" (str tag))) 
        new-program (list (list tagging-instruction
                                (code-at-point old-program index-to-tag))
                          (insert-code-at-point old-program
                                                index-to-tag
                                                tag-ref-instruction))]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))

(defn tag-branch-insertion-mutate 
  "Returns a version of the given individual with a tag-branch inserted at a random
   location. A tag-branch is a sequence of instructions that 1) produces a boolean
   value by performing a randomly chosen comparison of copies (not popped) of the top 
   two items of a randomly selected type, and 2) branches to one of two tags depending
   on the result. The tag-branch-mutation-type-instruction-pairs argument should be a sequence of pairs,
   in which the first element of each is a type and the second element is a Push instruction
   that performs a comparison of the type, as in [:integer 'integer_eq]."
  [ind tag-limit {:keys [max-points tag-branch-mutation-type-instruction-pairs maintain-ancestors]}]
  (let [old-program (:program ind)
        tag-ref-instruction-1 (symbol (str "tagged_" (str (lrand-int tag-limit)))) 
        tag-ref-instruction-2 (symbol (str "tagged_" (str (lrand-int tag-limit))))
        [type instruction] (lrand-nth tag-branch-mutation-type-instruction-pairs)
        yankdup-instruction (symbol (str (apply str (rest (str type))) "_yankdup"))
        tag-branch (list 1 yankdup-instruction 1 yankdup-instruction instruction 'exec_if
                         tag-ref-instruction-1 tag-ref-instruction-2)
        new-program (insert-randomly tag-branch old-program)]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some utilities are required for gaussian mutation

(defn gaussian-noise-factor
  "Returns gaussian noise of mean 0, std dev 1."
  []
  (*' (Math/sqrt (*' -2.0 (Math/log (lrand))))
     (Math/cos (*' 2.0 Math/PI (lrand)))))

(defn perturb-with-gaussian-noise 
  "Returns n perturbed with std dev sd."
  [sd n]
  (+' n (*' sd (gaussian-noise-factor))))

(defn perturb-code-with-gaussian-noise
  "Returns code with each float literal perturbed with std dev sd and perturbation probability
   per-num-perturb-probability."
  [code per-num-perturb-probability sd]
  (postwalklist (fn [item]
                  (if (and (float? item)
                           (< (lrand) per-num-perturb-probability))
                    (perturb-with-gaussian-noise sd item)
                    item))
                code))

(defn gaussian-mutate 
  "Returns the given individual where each float literal has a
   gaussian-mutation-per-number-mutation-probability chance of being gaussian
   mutated with a standard deviation of gaussian-mutation-standard-deviation."
  [ind {:keys [gaussian-mutation-per-number-mutation-probability
               gaussian-mutation-standard-deviation
               maintain-ancestors]}]
  (make-individual 
    :program (perturb-code-with-gaussian-noise
               (:program ind)
               gaussian-mutation-per-number-mutation-probability
               gaussian-mutation-standard-deviation)
    :history (:history ind)
    :ancestors (if maintain-ancestors
                 (cons (:program ind) (:ancestors ind))
                 (:ancestors ind))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ULTRA (Uniform Linear Transformation with Repair and Alternation) operator

(defn remove-empties
  "Removes empty sequences from tree t."
  [t]
  (prewalkseq
    (fn [node] (if (seq? node)
                 (remove #(and (seq? %) (empty? %)) node)
                 node))
    t))

(defn remove-ultra-padding 
  "Removes instances of 'ultra-padding from tree t."
  [t]
  (if (= t 'ultra-padding)
    '()
    (prewalkseq
      (fn [node] (if (seq? node)
                   (remove #{'ultra-padding} node)
                   node))
      t)))

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
        :else (let [s (str sequence)
                    l (read-string (string/replace (string/replace s ":open" " ( ") ":close" " ) "))]
                ;; there'll be an extra ( ) around l, which we keep if the number of read things is >1
                (if (= (count l) 1) 
                  (first l)
                  l))))
    
;(open-close-sequence-to-list '(:open 1 2 :open a b :open c :close :open :open d :close :close e :close :close))
;(open-close-sequence-to-list '(:open 1 :close :open 2 :close))
;(open-close-sequence-to-list '(:open :open 1 :close :open 2 :close :close))

(defn insert-somewhere 
  [thing lst]
  (let [after-how-many (lrand-int (inc (count lst)))]
    (concat (take after-how-many lst) 
            (list thing) 
            (drop after-how-many lst))))

(defn delete-somewhere 
  [thing lst]
  (let [locations (->> lst
                       (map vector (iterate inc 0))
                       (filter #(= (second %) thing))
                       (map first))
        location (lrand-nth locations)]
    (concat (take location lst)
            (drop (inc location) lst))))

;(delete-somewhere :right '(0 :right 1 2 :right))

(defn left-balance
  [s left right]
  (loop [processed ()
         to-process s
         extra-lefts 0]
    (cond 
      ;; done
      (empty? to-process)
      processed
      ;; see a left -- keep track of number unmatched
      (= (first to-process) left)
      (recur (concat processed (list (first to-process)))
             (rest to-process)
             (inc extra-lefts))
      ;; see a right -- may require fixes
      (= (first to-process) right)
      (if (zero? extra-lefts)
        ;; must fix
        (if (< (lrand) 0.5)
          ;; half the time delete a right (possibly this one) and continue
          (recur () 
                 (concat (delete-somewhere right (concat processed (list right))) 
                         (rest to-process))
                 0)
          ;; other half, add a left somewhere 
          (recur () (concat (insert-somewhere left processed) to-process) 0))
        ;; don't have to fix, keep going with adjusted extras
        (recur (concat processed (list (first to-process)))
               (rest to-process)
               (dec extra-lefts)))
      ;; anything else -- just keep going
      :else
      (recur (concat processed (list (first to-process)))
             (rest to-process)
             extra-lefts))))

(defn balance
  [open-close-sequence]
  ;(println "balancing:" open-close-sequence)
  (-> open-close-sequence
      (left-balance :open :close)
      (reverse)
      (left-balance :close :open)
      (reverse)))

;(let [s '(:open 1 2 :open a b :open c :close :open :open d :close :close e :close :close)]
;  (println s)
;  (println (balance s)))
;
;(let [s '(:open 1 2 :open :open a b :open c :close :open :open d :close :close e :close :close)]
;  (println s)
;  (println (balance s)))
;
;(let [s '(:open :open :close)]
;  (println s)
;  (println (balance s)))
;
;(let [s '(:close :close)]
;  (println s)
;  (println (balance s)))
;
;(let [s '(:close :open)]
;  (println s)
;  (println (balance s)))

(defn alternate
  [s1 s2 alternation-rate alignment-deviation max-points]
  (let [s1 (vec s1)
        s2 (vec s2)]
    (loop [i 0
           use-s1 (lrand-nth [true false])
           result []]
      (if (or (>= i (count (if use-s1 s1 s2)))
              (> (count result) (* 2 max-points))) ;; runaway growth
        (apply list result)
        (if (< (lrand) alternation-rate)
          (recur (max 0 (+' i (Math/round (*' alignment-deviation (gaussian-noise-factor)))))
                 (not use-s1)
                 result)
          (recur (inc i)
                 use-s1
                 (conj result (nth (if use-s1 s1 s2) i))))))))

; (alternate '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) '(a b c d e f g h i j k) 0.2 1)

(defn linearly-mutate
  [open-close-sequence mutation-rate use-ultra-no-paren-mutation atom-generators]
  (let [parentheses (if @global-use-bushy-code
                      (let [n (count atom-generators)]
                        (concat (repeat n :open) (repeat n :close)))
                      [:open :close])
        token-mutator (fn [token]
                        (if (and (or (not use-ultra-no-paren-mutation) ;if not using no-paren mutation
                                     (not (some #{token} [:open :close]))) ;or if the token isn't :open or :close
                                 (< (lrand) mutation-rate)) ;and if randomly below mutation rate
                          (random-code 1 (concat atom-generators (if use-ultra-no-paren-mutation ;random instruction, including parens if not no-paren mutation
                                                                   []
                                                                   parentheses)))
                          ;; NOTE: The original ULTRA mutation (first 2 papers) allowed replacement with (), which would later be removed,
                          ;; which is essentially deletion. This was removed later since it biases ULTRA toward smaller programs. This looked like:  ;(concat atom-generators parentheses [()]))
                          token))] ;else, just return the token
    (map token-mutator
         open-close-sequence)))

(defn ultra-operate-on-programs
  [p1 p2 alternation-rate alignment-deviation mutation-rate
   use-ultra-no-paren-mutation ultra-pads-with-empties atom-generators
   max-points]
  (if (or (not (seq? p1))
          (not (seq? p2)))
    p1 ;; can't do if either program isn't a list
    (let [p1 (if (>= (count p1) (count p2))
               p1
               (concat p1 (repeat (- (count p2) (count p1)) 
                                  (if ultra-pads-with-empties () 'ultra-padding))))
          p2 (if (>= (count p2) (count p1))
               p2
               (concat p2 (repeat (- (count p1) (count p2)) 
                                  (if ultra-pads-with-empties () 'ultra-padding))))]
      ((if ultra-pads-with-empties remove-empties remove-ultra-padding)
        (open-close-sequence-to-list
          (balance
            (linearly-mutate
              (alternate (list-to-open-close-sequence p1)
                         (list-to-open-close-sequence p2)
                         alternation-rate
                         alignment-deviation
                         max-points)
              mutation-rate
              use-ultra-no-paren-mutation
              atom-generators)))))))

;(let [p1 '(a (b c) (d ((e)) f))
;      p2 '((1 2 3) 4 ((5)) 6)]
;  (println p1)
;  (println p2)
;  (println (ultra-operate-on-programs p1 p2 0.2 1 0.1 false ['X])))

;(loop [i 0
;       pgm-a (random-code-with-size 30 '(a))
;       pgm-b (random-code-with-size 30 '(b))]
;  (println "pgm-a:" pgm-a)
;  (println "pgm-b:" pgm-b)
;  (if (< i 100)
;    (recur (inc i)
;           (ultra-operate-on-programs pgm-a pgm-b 0.2 1 0.1 false '(a))
;           (ultra-operate-on-programs pgm-b pgm-a 0.2 1 0.1 false '(b)))))

(defn ultra
  "Returns the result of applying the ULTRA (Uniform Linear Transformation
   with Repair and Alternation) operation to parent1 and parent2."
  [parent1 parent2 {:keys [max-points ultra-alternation-rate ultra-alignment-deviation
                           ultra-mutation-rate atom-generators maintain-ancestors
                           use-ultra-no-paren-mutation ultra-pads-with-empties]}]
  (let [new-program (ultra-operate-on-programs (:program parent1)
                                               (:program parent2)
                                               ultra-alternation-rate
                                               ultra-alignment-deviation
                                               ultra-mutation-rate
                                               use-ultra-no-paren-mutation
                                               ultra-pads-with-empties
                                               atom-generators
                                               max-points)]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:program parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))
