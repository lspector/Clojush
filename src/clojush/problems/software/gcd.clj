;; gcd.clj
;; Bill Tozier, bill@vagueinnovation.com
;; updated February 24, 2020
;;
;; This is code for finding algorithms which return the greatest common
;;   divisor (GCD) of a pair of integer arguments. See the Wikipedia
;;   article for background:
;;     https://en.wikipedia.org/wiki/Greatest_common_divisor
;;
;; The two inputs are given as integers using the :input stack, and the result
;; is read from the top of :integer.
;;
;; Some evolved solutions are saved in comments at the end of this file

(ns clojush.problems.software.gcd
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util]
        [clojure.math.numeric-tower]
        ))


; Convenience function to generate test cases
(defn product-of-n-d-sided-dice
  "Produce the product of n randomly-chosen integers from [1,d]"
  [n d]
  (reduce *' 1 (repeatedly n #(inc (rand-int d)))
    ))


; error function
(defn nontrivial-gcd-training-case
  "Given integer scale and multiplicity arguments, produce a triple of two (possibly huge) integers and their actual GCD"
  [scale multiplicity]
  (let [a (product-of-n-d-sided-dice multiplicity scale)
        b (product-of-n-d-sided-dice multiplicity scale)]
        [a b (gcd a b)]
        ))


(defn gcd-coprime-case
  "Given a first integer argument, we scan upwards (by random increments of 1-100) until we find a second argument which is coprime with the first integer; that is, where the pair has GCD=1"
  [arg1]
  (loop [arg2 (+ arg1 (rand-int 100) 1)]
    (if (= 1 (gcd arg1 arg2))
      [arg1 arg2 (gcd arg1 arg2)]
      (recur (+ arg2 (rand-int 100) 1))
      )))


(defn identical-case
  "You can figure this out, I just know it."
  [n]
  [n n n]
  )


(def gcd-training-cases
  "Produces a random sample of training cases: some cases where the integer arguments are identical, some pairs of moderate integers (~10000) with nontrivial GCD values, some pairs of large integers (~10^8) with nontrivial GCD values, some Very Large pairs, and some pairs of coprime integers with GCD=1"
  (into []
    (concat
      (repeatedly 5 #(identical-case (product-of-n-d-sided-dice 20 7)))
      (repeatedly 40 #(nontrivial-gcd-training-case 7 7))
      (repeatedly 30 #(nontrivial-gcd-training-case 20 7))
      (repeatedly 20 #(nontrivial-gcd-training-case 20 11))
      (repeatedly 5
        #(gcd-coprime-case (product-of-n-d-sided-dice 20 7)))
        )))


(defn abs-err
  [observed expected]
  (abs (-' observed expected)))


(defn gcd-error-function
  "Given an individual and a collection of training cases, we run the individual's program using the training cases' inputs, and return a vector of absolute errors comparing the observed vs expected GCD values. The behavior (observed results) and error (absolute difference) vectors are associated with the individual as a result."
  [individual cases]
  (let [behaviors (vec (for [a-case cases]
                         (->> (make-push-state)
                              (push-item , (first a-case) :input)
                              (push-item , (nth a-case 1) :input)
                              (run-push , (:program individual))
                              (top-item , :integer))))
        errors (mapv (fn [behavior a-case]
                       (if (int? behavior)
                        (abs-err behavior (last a-case))
                        1e15))
                     behaviors
                     cases)]
    (assoc individual
      :behaviors behaviors
      :errors errors)))


(defn gcd-initial-report
  [argmap]
  (println "Training cases:")
  (doseq [[i case] (map vector (range) gcd-training-cases)]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))



; Atom generators
(def gcd-atom-generators
  "Collection of items which will be used to construct random programs: Some integer constants, boolean constants, the input and various standard Push instructions. The relative proportions are stupid and simply the result of poking and prodding with insufficient evidence; feel free to change them."
  (concat
    (repeat 10 'in1)
    (repeat 10 'in2)
    (list
      10
      1000
      1000000
      1000000000
      2
      3
      5
      7
      11
      19
      31
      53
      true
      false
      ;; constants
      (fn [] (- (lrand-int 21) 10))    ;; Integer ERC [-10,10]
      (fn [] (- (lrand-int 2001) 1000))  ;; Integer ERC [-1000,1000]
      ;;; end input instructions
      )
    (registered-for-stacks [:integer :boolean :exec :code])
      ))



; Define the argmap
(def argmap
  "I literally have no idea what many of these things do. However, with the current settings, the first run I attempted did solve the problem to my satisfaction. YMMV."
  {:error-function (fn [individual]
                        (gcd-error-function
                          individual
                          gcd-training-cases
                          ))
     :atom-generators gcd-atom-generators
     :max-points 1000
     :max-genome-size-in-initial-program 500
     :evalpush-limit 1000
     :population-size 400
     :max-generations 5000
     :parent-selection :lexicase
     :genetic-operator-probabilities {:uniform-addition-and-deletion 0.5
                                      :alternation 0.5}
     :uniform-addition-and-deletion-rate [0.001 0.01 0.1]
     :alternation-rate [0.001 0.01 0.1]
     :alignment-deviation [0 1 10 100]
     :report-simplifications 0
     :final-report-simplifications 5000
     :problem-specific-initial-report gcd-initial-report

     })

;; Evolved solutions (not all simplified):

;; (in2 in1 integer_mod in1 in1 integer_dup_items integer_mod integer_mod 53
;; integer_dup_items in1 integer_mod in2 integer_mod integer_mod 2
;; integer_dup_items integer_mod integer_swap 2 integer_dup_items integer_dec
;; integer_dec integer_mod integer_swap 2 integer_dup_items 2 integer_dup_items
;; integer_mod integer_mod 2 integer_dup_items integer_mod 2 integer_dup_items
;; integer_mod integer_swap 2 integer_dup_items integer_dec integer_dec
;; integer_mod integer_mod 2 integer_dup_items integer_mod 2 integer_dup_items
;; integer_mod integer_swap 2 integer_dup_items integer_dec integer_dec
;; integer_mod integer_mod 2 integer_dup_items integer_mod 2 integer_dup_items
;; integer_mod 2 integer_dup_items integer_mod exec_dup_times ())

;; program: (in1 in2 integer_sub in2 integer_sub in2 in1 in2 integer_mod
;; integer_mod in1 in2 in2 in1 in2 integer_mod integer_mod integer_mod
;; integer_mod integer_mod integer_mod exec_eq in1 boolean_and in1 in2
;; integer_yankdup integer_sub integer_mod integer_lte in1 integer_mod
;; integer_mod in1 integer_mod in2 in1 in2 in2 boolean_dup_items integer_mod
;; integer_mod exec_eq code_yankdup in2 in2 integer_swap integer_dup_items
;; integer_mod integer_mod integer_sub in2 integer_mod integer_lte in1
;; integer_mod in2 in1 in2 integer_mod integer_mod integer_mod integer_lte in1
;; in2 integer_yankdup exec_swap integer_sub integer_mod integer_mod in2 in1 in2
;; integer_mod in1 boolean_dup_items integer_mod in2 integer_dup_items
;; integer_mod integer_mod integer_mod integer_sub in1 in2 integer_dup_items
;; integer_mod integer_mod integer_sub integer_lte in2 integer_mod integer_mod
;; integer_lte in2 in2 integer_dup_items integer_mod integer_mod integer_sub in2
;; integer_mod integer_lte in1 in2 integer_mod integer_mod in2 in1 in2 in1
;; boolean_dup_items integer_mod integer_mod in2 integer_dup_items integer_mod
;; integer_mod in2 integer_dup_items integer_mod integer_mod 7 integer_dup_items
;; exec_dup_times integer_mod)

;; program: (in1 in2 in1 exec_dup_items exec_dup (exec_dup (integer_mod
;; exec_empty exec_dup (integer_min exec_if integer_mod integer_sub integer_gte
;; in2 in1 in2 integer_yankdup))))

;; Successful program: (integer_lt code_subst in1 true in1 in1 boolean_rot
;; code_position code_swap code_quote (code_noop integer_inc) exec_eq
;; integer_yankdup integer_pop code_subst in2 in2 in1 boolean_rot exec_dup
;; (exec_do*while (boolean_or code_quote (code_do* integer_yankdup)) boolean_xor
;; boolean_rot) exec_when (code_subst in1 code_stackdepth code_flush integer_min
;; in1 exec_eq in1 integer_yankdup) code_flush code_flush code_subst in2
;; code_null in1 in2 in1 1000000000 code_length code_length exec_dup_times
;; (boolean_yank integer_stackdepth integer_dup_items exec_dup_times
;; (integer_dup_items integer_stackdepth integer_dup_items exec_dup_times
;; (integer_dup_items integer_mod integer_empty integer_rot))))

;; Successful program: (exec_swap (exec_dup (3 integer_dup_items) integer_add)
;; (code_do*count exec_when (code_frominteger exec_swap (boolean_or exec_if
;; (code_extract exec_dup_times (in2 exec_k (boolean_invert_first_then_and
;; boolean_shove in1 in1 in2) () integer_pop) 1000000) (integer_dup_items) 1000
;; exec_swap () (code_fromboolean in1 in1 in2 integer_eq exec_while (in2
;; integer_dup_items boolean_xor exec_flush in1 integer_gt in2)) in2
;; integer_empty exec_dup_items exec_swap (integer_pop boolean_and boolean_yank
;; integer_yankdup integer_inc exec_k (exec_dup_items integer_mod) ()) ()) ())))

;; Successful program: (in1 boolean_rot in1 in2 in2 boolean_rot code_atom
;; exec_dup_items integer_sub code_subst in1 in2 code_position integer_dup_items
;; integer_mod integer_mod code_position code_atom code_atom boolean_empty
;; code_extract integer_sub boolean_invert_first_then_and
;; boolean_invert_first_then_and integer_eq in2 in1 in2 in1 code_length in1
;; integer_lt integer_sub code_dup_items integer_mod integer_mod integer_mod
;; boolean_rot integer_sub integer_lt code_pop code_extract integer_sub
;; code_extract integer_sub integer_eq in2 in1 in2 in1 code_length in1
;; code_dup_items integer_mod integer_mod in1 in2 integer_mod integer_mod in1
;; in2 integer_mod integer_mod code_noop code_subst in1 in2 boolean_rot
;; exec_dup_items integer_sub integer_lt in1 integer_mod integer_mod boolean_dup
;; integer_mod in2 integer_mod boolean_dup in2 in1 integer_dup_items integer_mod
;; integer_mod code_do code_position integer_sub integer_lt code_position
;; boolean_eq integer_mod code_dup_times exec_noop in2 integer_mod exec_noop
;; integer_mod code_eq)

;; Successful program: (in1 boolean_invert_first_then_and integer_mod
;; code_append exec_when (in2) in1 integer_yankdup code_do*times in2 integer_mod
;; code_append in2 in1 code_do*times in2 code_dup_times in1 integer_empty
;; integer_mod code_list boolean_empty integer_mod in1 exec_dup () in2
;; integer_mod exec_dup_items integer_mod code_yank integer_mod code_list
;; integer_mod integer_mod integer_yank integer_mod integer_yank in2 code_atom
;; boolean_invert_first_then_and integer_mod boolean_invert_first_then_and in1
;; in1 integer_mult code_subst code_do*range boolean_empty integer_mod code_list
;; in1 integer_mod integer_mod integer_yank in2 boolean_invert_first_then_and
;; code_atom integer_mod in1 in1 integer_mult code_subst in2 1000000 exec_eq
;; code_flush code_flush 19 integer_mod code_atom boolean_yank in1 exec_while
;; (exec_empty) integer_mod integer_swap integer_yankdup integer_mod integer_mod
;; integer_max integer_mod code_yank in2 integer_mod in2 integer_mod integer_max
;; integer_mod in2 boolean_dup integer_mod code_subst code_yank in2 integer_mod
;; boolean_invert_first_then_and integer_max code_map code_if in2 in1 in2
;; integer_mod integer_mod code_list in1 integer_yankdup integer_mod integer_mod
;; integer_mod integer_max code_null in1 code_yank code_nth code_list
;; integer_mod integer_max in1 code_map code_append in2 in1 integer_mod
;; integer_mod code_list in1 integer_yankdup integer_mod integer_mod
;; boolean_invert_first_then_and integer_yank integer_max in1 code_insert
;; code_map code_append code_if code_if in2 in1 integer_mod integer_mod
;; code_list in1 integer_yankdup integer_mod integer_mod integer_mod integer_mod
;; boolean_invert_first_then_and integer_yank integer_max in1 code_map
;; code_append code_if code_append code_if code_if in2 in1 integer_mod
;; integer_mod code_list in1 integer_yankdup integer_mod integer_mod integer_mod
;; integer_mod code_noop integer_max code_yank integer_mod integer_max in1
;; code_yank code_map code_append code_if in2 in1 code_if in2 code_yank
;; integer_mod integer_max in1 code_yank boolean_rot integer_yankdup integer_mod
;; integer_mod integer_mod integer_max integer_mod in2 integer_mod integer_max
;; integer_max code_yank in2 in2 integer_mod code_nthcdr in2 integer_max in1
;; code_map code_append in2 in1 integer_mod integer_mod code_list
;; integer_yankdup integer_mod integer_mod integer_mod code_rot in1 code_map
;; code_append in2 in1 integer_mod integer_mod code_list in1 integer_yankdup
;; integer_mod integer_mod integer_mod integer_mod integer_max in1 code_yank
;; code_list integer_mod integer_max)

;; SUCCESS at generation 540
;; Successful program: (in1 in2 code_append integer_mod integer_mod in2 in2
;; code_do* boolean_dup integer_yankdup in1 integer_min integer_mod integer_mod
;; integer_max integer_empty in1 in1 code_cons code_insert integer_dup_items
;; integer_yankdup code_swap boolean_dup integer_min integer_mod integer_sub
;; integer_mod integer_mod integer_eq in2 in2 in2 integer_yankdup false
;; code_null integer_min integer_mod integer_sub code_member integer_mod in2 in2
;; in2 boolean_dup integer_yankdup false code_null boolean_dup integer_min
;; integer_mod integer_mod integer_max integer_add in1 in1 in2 boolean_swap
;; integer_yankdup 11 integer_max integer_mod exec_pop (integer_mod) integer_max
;; code_size integer_mod code_position integer_gte integer_max code_shove
;; exec_if () (integer_min integer_gte) integer_mod code_do*range integer_mod
;; integer_max integer_empty in1 in1 in1 code_cons integer_yankdup boolean_dup
;; integer_min integer_mod code_extract integer_sub integer_mod integer_add in2
;; exec_dup_items in2 exec_if () (integer_min integer_swap) integer_mod code_nth
;; integer_mod integer_mod integer_max integer_add in1 boolean_pop in1 in1
;; code_cons integer_yankdup boolean_invert_second_then_and boolean_dup
;; integer_min integer_mod integer_sub integer_mod integer_mod integer_mod in2
;; in2 in2 boolean_or boolean_dup integer_yankdup integer_min integer_mod
;; integer_max integer_add in1 in1 integer_min integer_mod 1000000000
;; boolean_rot boolean_not integer_mod integer_max in1 integer_dup_items in2 in2
;; in2 in1 7 in2 integer_mod integer_mod integer_add in2 in2 boolean_shove
;; boolean_dup integer_min integer_gt exec_shove (boolean_xor integer_mod)
;; integer_max integer_empty in1 in1 in1 code_cons code_rot integer_yankdup
;; integer_min integer_mod integer_sub integer_mod integer_mod in2 in2
;; integer_mod integer_sub integer_mod integer_mod in2 in2 boolean_swap
;; code_atom in1 in1 code_cons boolean_dup boolean_or integer_min integer_mod
;; integer_sub integer_mod integer_mod integer_add in2 in2 code_if boolean_swap
;; code_atom integer_yankdup in1 code_cons boolean_dup boolean_or integer_min
;; integer_mod integer_sub integer_mod integer_add in2 in2 in1 code_cons
;; integer_yankdup code_append boolean_dup integer_min integer_mod integer_sub
;; integer_mod integer_mod code_dup_times in2 in2 in2 integer_yankdup
;; boolean_dup boolean_xor integer_min integer_mod code_length integer_mod
;; integer_add boolean_eq in1 in1 in1 integer_yankdup boolean_dup integer_min
;; integer_mod integer_sub integer_mod integer_dup_times integer_max in1
;; code_quote (exec_y (exec_empty 1000 11 7)))
