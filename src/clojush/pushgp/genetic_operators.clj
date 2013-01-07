(ns clojush.pushgp.genetic-operators
  (:use [clojush.util]
        [clojush.random]
        [clojush.globals]
        [clojush.individual]
        [clojush.node-selection]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; genetic operators

(defn mutate 
  "Returns a mutated version of the given individual."
  [ind mutation-max-points max-points atom-generators]
  (let [new-program (insert-code-at-point (:program ind) 
                                          (select-node-index (:program ind))
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
  [parent1 parent2 max-points]
  (let [new-program (insert-code-at-point 
                      (:program parent1) 
                      (select-node-index (:program parent1))
                      (code-at-point (:program parent2)
                                     (select-node-index (:program parent2))))]
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
  [parent1 parent2 new-code-max-points max-points atom-generators]
  (let [new-program (list (random-code new-code-max-points atom-generators) 
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
  [ind]
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
  [ind max-points]
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
  [ind max-points tag-limit]
  (let [old-program (:program ind)
        index-to-tag (select-node-index old-program)
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
on the result. The type-instruction-pairs argument should be a sequence of pairs,
in which the first element of each is a type and the second element is a Push instruction
that performs a comparison of the type, as in [:integer 'integer_eq]."
  [ind max-points type-instruction-pairs tag-limit]
  (let [old-program (:program ind)
        tag-ref-instruction-1 (symbol (str "tagged_" (str (lrand-int tag-limit)))) 
        tag-ref-instruction-2 (symbol (str "tagged_" (str (lrand-int tag-limit))))
        [type instruction] (lrand-nth type-instruction-pairs)
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
;; some utilities are required for amalgamation crossover

(defn remove-empties 
  "Removes empty sequences from tree t."
  [t]
  (clojure.walk/postwalk 
    (fn [node] (if (seq? node) 
                 (remove #(and (seq? %) (empty? %)) node)
                 node))
    t))

(defn combine [t1 t2] 
  "Returns a list containing all of the elements of t1 and t2, after making each into
a list if necessary."
  (apply list (concat (ensure-list t1) (ensure-list t2))))

(defn self-or-other-or-both-or-neither [self other]
  (let [n (lrand)]
    (cond (< n (:self @global-amalgamation-parameters)) self
          (< n (+ (:self @global-amalgamation-parameters)
                  (:other @global-amalgamation-parameters))) other
          (< n (+ (:self @global-amalgamation-parameters)
                  (:other @global-amalgamation-parameters)
                  (:self-other @global-amalgamation-parameters))) (list self other)
          (< n (+ (:self @global-amalgamation-parameters)
                  (:other @global-amalgamation-parameters)
                  (:self-other @global-amalgamation-parameters)
                  (:other-self @global-amalgamation-parameters))) (list other self)
          :else ())))

(defn null? [thing] (and (seq? thing) (empty? thing)))

(defn amalgamate
  [t1 t2]
  (remove-empties
    (if (or (null? t1)
            (not (seq? t1))
            (null? t2))
      (self-or-other-or-both-or-neither t1 t2)
      (map amalgamate 
           t1 
           (if (seq? t2) 
             (cycle t2) 
             (cycle [t2]))))))

;; an example of amalgamating two lists with the same structure
#_(do ;; print self first for comparison
  (println '(a (b ((c d) e) f (g h i (j k l) ((m n o p q r) s t u v ((w) x y z))))))
  (println (amalgamate 
             '(a (b ((c d) e) f (g h i (j k l) ((m n o p q r) s t u v ((w) x y z)))))
             '(A (B ((C D) E) F (G H I (J K L) ((M N O P Q R) S T U V ((W) X Y Z))))))))

;; an example of amalgamating two lists with the same structure
#_(do ;; print self first for comparison
  (println '(a (b ((c d) e) f (g h i (j k l) ((m n o p q r) s t u v ((w) x y z))))))
  (amalgamate '(a (b ((c d) e) f (g h i (j k l) ((m n o p q r) s t u v ((w) x y z)))))
            '((1 2) 3 (4 ((5)) 6 7 8 9 10 11)(12 13 14 (15 16 17 18)(19 20) 21 22 23))))

(defn amalgamation-crossover 
  "Amalgamation."
  [parent1 parent2 max-points]
  (let [new-program (amalgamate (:program parent1) (:program parent2))]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:program parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some utilities are required for gaussian mutation

(defn gaussian-noise-factor
  "Returns gaussian noise of mean 0, std dev 1."
  []
  (* (Math/sqrt (* -2.0 (Math/log (lrand))))
     (Math/cos (* 2.0 Math/PI (lrand)))))

(defn perturb-with-gaussian-noise 
  "Returns n perturbed with std dev sd."
  [sd n]
  (+' n (* sd (gaussian-noise-factor))))

(defn perturb-code-with-gaussian-noise
  "Returns code with each float literal perturbed with std dev sd and perturbation probability
   num-perturb-probability."
  [code per-num-perturb-probability sd]
  (postwalklist (fn [item]
                  (if (and (float? item)
                           (< (lrand) per-num-perturb-probability))
                    (perturb-with-gaussian-noise sd item)
                    item))
                code))

(defn gaussian-mutate 
  "Returns a gaussian-mutated version of the given individual."
  [ind per-num-perturb-probability sd]
  (make-individual 
    :program (perturb-code-with-gaussian-noise (:program ind) per-num-perturb-probability sd)
    :history (:history ind)
    :ancestors (if maintain-ancestors
                 (cons (:program ind) (:ancestors ind))
                 (:ancestors ind))))
