
;; nimrod_row1.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2012

(ns experimental.nimrod_row1
  (:use [clojush] [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Integer symbolic regression for the integer sequence obtained from row 1
;; of the nimrod table produced by Henle's table.py (see Henle and Schlatter).

(define-registered n 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(define-registered prev
  (fn [state] (push-item (stack-ref :auxiliary 1 state) :integer state)))

(def table-row-0
  [0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0]
  )

(def integer-sequence 
  (loop [s [] 
         r (drop 2 table-row-0 )
         i 1]
    (if (empty? r)
      s
      (if (= 1 (first r))
        (recur (conj s i) (rest r) (inc i))
        (recur s (rest r) (inc i))))))
  
;integer-sequence
  
#_(def fitness-cases
  (doall (map vector
              (iterate inc 1)
              [3 5 8 11 13 17 20 22 25 27 30 32 35 38 40 43]))) ;; wrong! (Henle says paper was wrong)

(def fitness-cases
  (doall (map vector
              (iterate inc 1)
              integer-sequence)))

(defn previous [n]
  (loop [s (reverse integer-sequence)]
    (if (< (count s) 2)
      0
      (if (= n (first s))
        (second s)
        (recur (rest s))))))

(define-registered prev
  (fn [state] (push-item (stack-ref :auxiliary 1 state) :integer state)))

;(doall (map previous integer-sequence))

#_(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [[input target] fitness-cases]
                        (let [state (run-push program 
                                              (push-item (previous input) :auxiliary 
                                                         (push-item input :auxiliary 
                                                                    (make-push-state))))
                              top-int (top-item :integer state)]
                          (if (and (number? top-int) (< -1000000 top-int 1000000))
                            (Math/abs (- (int top-int) target))
                            1000)))))
  :atom-generators (concat [(fn [] (lrand-int 10))
                            'n
                            'prev
                            ]
                           '(
                              ;code_nthcdr
                              ;exec_y
                              ;code_insert
                              ;zip_insert_left_fromexec
                              ;zip_shove
                              ;float_lt
                              ;exec_fromziprights
                              ;code_fromfloat
                              ;boolean_swap
                              integer_add
                              ;code_stackdepth
                              ;integer_eq
                              ;zip_replace_fromcode
                              ;exec_pop
                              integer_swap
                              ;zip_yank
                              ;code_noop
                              ;float_rot
                              ;boolean_eq
                              ;string_pop
                              ;float_yank
                              ;code_subst
                              ;string_take
                              ;zip_fromcode
                              ;code_overlap
                              ;code_yankdup
                              ;float_sin
                              ;zip_end?
                              ;code_fromziprights
                              integer_yank
                              ;boolean_yank
                              ;zip_insert_right_fromexec
                              ;zip_insert_child_fromexec
                              ;float_frominteger
                              ;zip_swap
                              ;boolean_fromfloat
                              ;zip_yankdup
                              ;string_eq
                              ;zip_insert_left_fromcode
                              ;exec_eq
                              ;zip_fromexec
                              integer_dup
                              ;float_cos
                              ;float_stackdepth
                              ;zip_stackdepth
                              ;zip_right
                              ;code_null
                              integer_yankdup
                              ;zip_append_child_fromexec
                              ;code_pop
                              ;code_swap
                              ;float_swap
                              ;code_append
                              ;code_member
                              ;boolean_flush
                              ;code_do*
                              ;code_dup
                              ;code_quote
                              ;boolean_rot
                              ;boolean_and
                              ;code_shove
                              ;integer_lt
                              integer_flush
                              ;exec_stackdepth
                              ;exec_rot
                              ;string_stackdepth
                              ;code_cons
                              ;string_rot
                              ;float_div
                              ;code_container
                              ;code_if
                              ;exec_when
                              ;exec_do*times
                              ;code_extract
                              integer_shove
                              ;float_rand
                              ;boolean_rand
                              ;float_shove
                              ;code_wrap
                              integer_mult
                              ;zip_prev
                              integer_stackdepth
                              ;float_sub
                              integer_div
                              ;code_fromziproot
                              ;code_nth
                              ;code_discrepancy
                              ;zip_replace_fromexec
                              ;exec_do*count
                              ;code_size
                              ;zip_up
                              ;string_rand
                              ;exec_s
                              ;code_length
                              ;boolean_shove
                              ;code_cdr
                              ;zip_dup
                              ;exec_do*range
                              ;exec_fromzipnode
                              ;exec_if
                              ;float_flush
                              ;zip_eq
                              ;code_map
                              ;float_yankdup
                              ;code_rand
                              ;exec_fromziplefts
                              ;integer_gt
                              ;integer_max
                              ;code_atom
                              ;exec_fromzipchildren
                              ;zip_remove
                              ;zip_flush
                              ;float_fromboolean
                              ;code_contains
                              ;boolean_not
                              ;integer_fromfloat
                              ;code_list
                              ;boolean_or
                              ;code_do*range
                              ;exec_k
                              ;integer_fromboolean
                              ;code_fromzipnode
                              ;boolean_frominteger
                              ;float_gt
                              ;exec_yank
                              ;float_add
                              ;float_tan
                              ;code_eq
                              ;string_yank
                              ;zip_pop
                              ;code_fromzipchildren
                              ;float_mult
                              ;boolean_stackdepth
                              ;zip_left
                              ;code_flush
                              integer_sub
                              ;zip_branch?
                              ;string_swap
                              integer_mod
                              ;zip_append_child_fromcode
                              ;code_fromboolean
                              ;zip_rightmost
                              ;float_max
                              integer_rot
                              ;exec_flush
                              ;exec_yankdup
                              ;string_yankdup
                              ;exec_fromziproot
                              ;exec_swap
                              ;string_flush
                              ;zip_down
                              ;code_yank
                              ;string_length
                              ;float_pop
                              ;boolean_yankdup
                              ;zip_insert_child_fromcode
                              ;float_eq
                              ;code_frominteger
                              ;zip_leftmost
                              ;float_min
                              ;boolean_dup
                              ;string_concat
                              ;code_do*count
                              ;code_car
                              ;zip_insert_right_fromcode
                              ;exec_dup
                              ;integer_min
                              ;zip_next
                              ;string_shove
                              ;code_position
                              ;float_dup
                              ;integer_rand
                              ;exec_shove
                              ;string_dup
                              ;exec_noop
                              ;code_fromziplefts
                              ;code_do
                              integer_pop
                              ;boolean_pop
                              ;zip_rot
                              ;code_do*times
                              ;code_rot
                              ;float_mod
                              ))
  :use-single-thread false
  :max-points 150
  :mutation-probability 0.40
  :mutation-max-points 20
  :crossover-probability 0.40
  :simplification-probability 0.10
  :tournament-size 7
  :report-simplifications 100
  :final-report-simplifications 1000
  :reproduction-simplifications 10
  :trivial-geography-radius 0
  :decimation-ratio 1
  :decimation-tournament-size 2
  :evalpush-limit 300
  :evalpush-time-limit 0
  :node-selection-method :size-tournament
  :node-selection-leaf-probability 0.1
  :node-selection-tournament-size 2
  :pop-when-tagging true
  :gaussian-mutation-probability 0.0
  :gaussian-mutation-per-number-mutation-probability 0.5
  :gaussian-mutation-standard-deviation 0.1
  :reuse-errors true
  :use-historically-assessed-hardness false
  :use-lexicase-selection true
  )

;; apparent solution


;(def soln '(9 3 integer_add prev integer_min prev prev integer_div prev integer_dup integer_stackdepth integer_add integer_rot integer_shove integer_stackdepth integer_stackdepth integer_rot 0 integer_add integer_yankdup integer_mult integer_stackdepth integer_yank integer_yankdup integer_add integer_rot integer_swap integer_stackdepth integer_div integer_swap integer_shove integer_add 3 n integer_max n integer_shove integer_sub 3 integer_mod prev integer_add integer_dup integer_stackdepth integer_add integer_stackdepth prev integer_add integer_yank integer_dup integer_yank 0 prev integer_stackdepth integer_rot prev integer_add integer_yank integer_yankdup integer_mult integer_rot integer_swap integer_stackdepth integer_div integer_rot integer_add prev prev integer_rot integer_stackdepth integer_div integer_rot integer_add prev prev integer_add integer_mod 2 integer_max prev integer_add)
;  )
;
;(defn err [program]
;  (doall
;    (for [[input target] fitness-cases]
;      (let [state (run-push program 
;                            (push-item (previous input) :auxiliary 
;                                       (push-item input :auxiliary 
;                                                  (make-push-state))))
;            top-int (top-item :integer state)]
;        (if (and (number? top-int) (< -1000000 top-int 1000000))
;          (Math/abs (- (int top-int) target))
;          1000)))))
;
;(reduce + (err soln))

;(def bigger-row
;  [0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0]
;   )
;
;(def bigger-sequence 
;  (loop [s [] 
;         r (drop 2 bigger-row )
;         i 1]
;    (if (empty? r)
;      s
;      (if (= 1 (first r))
;        (recur (conj s i) (rest r) (inc i))
;        (recur s (rest r) (inc i))))))
;  
;(def bigger-cases
;  (doall (map vector
;              (iterate inc 1)
;              bigger-sequence)))
;
;(defn bigger-err [program]
;  (doall
;    (for [[input target] bigger-cases]
;      (let [state (run-push program 
;                            (push-item (previous input) :auxiliary 
;                                       (push-item input :auxiliary 
;                                                  (make-push-state))))
;            top-int (top-item :integer state)]
;        (if (and (number? top-int) (< -1000000 top-int 1000000))
;          (Math/abs (- (int top-int) target))
;          1000)))))
;
;
;(reduce + (bigger-err soln))

;; another apparent solution

(def soln '(n prev n integer_sub integer_mod 4 integer_add 6 integer_mod 3 integer_yankdup integer_div prev integer_swap 6 integer_swap 3 integer_swap integer_shove integer_swap integer_sub integer_div 8 integer_yank prev integer_dup integer_sub prev prev prev integer_stackdepth integer_mod integer_mod integer_rot integer_pop integer_div integer_mod 8 n integer_rot integer_shove integer_sub integer_stackdepth integer_add integer_mod integer_stackdepth 3 integer_dup integer_sub integer_pop integer_yank 3 integer_dup integer_mult integer_pop integer_div 2 integer_stackdepth integer_stackdepth integer_shove prev integer_yankdup 5 integer_pop integer_mult integer_yank integer_shove integer_mult 1 integer_dup integer_div integer_sub integer_dup prev integer_dup integer_div integer_add integer_div integer_stackdepth prev 3 integer_mult prev 3 integer_mult integer_yank integer_add integer_dup integer_stackdepth integer_stackdepth integer_add integer_yank 8 integer_sub integer_div integer_add)
 )

(defn err [program]
  (doall
    (for [[input target] fitness-cases]
      (let [state (run-push program 
                            (push-item (previous input) :auxiliary 
                                       (push-item input :auxiliary 
                                                  (make-push-state))))
            top-int (top-item :integer state)]
        (if (and (number? top-int) (< -1000000 top-int 1000000))
          (Math/abs (- (int top-int) target))
          1000)))))

(reduce + (err soln))
;
;(def bigger-row
;  [0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0]
;   )
;
;(def bigger-sequence 
;  (loop [s [] 
;         r (drop 2 bigger-row )
;         i 1]
;    (if (empty? r)
;      s
;      (if (= 1 (first r))
;        (recur (conj s i) (rest r) (inc i))
;        (recur s (rest r) (inc i))))))
;  
;(def bigger-cases
;  (doall (map vector
;              (iterate inc 1)
;              bigger-sequence)))
;
;(defn bigger-err [program]
;  (doall
;    (for [[input target] bigger-cases]
;      (let [state (run-push program 
;                            (push-item (previous input) :auxiliary 
;                                       (push-item input :auxiliary 
;                                                  (make-push-state))))
;            top-int (top-item :integer state)]
;        (if (and (number? top-int) (< -1000000 top-int 1000000))
;          (Math/abs (- (int top-int) target))
;          1000)))))
;
;
;(reduce + (bigger-err soln))


