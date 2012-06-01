;; nimrod_matrix.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2012

(ns experimental.nimrod_matrix
  (:use [clojush] [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Integer symbolic regression for row r, column c of the nimrod matrix
;; produced by Henle's table.py (see Henle and Schlatter).

(define-registered r 
  (fn [state] (push-item (stack-ref :auxiliary 1 state) :integer state)))

(define-registered c 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(define-registered fr 
  (fn [state] (push-item (float (stack-ref :auxiliary 0 state)) :float state)))

(define-registered fc 
  (fn [state] (push-item (float (stack-ref :auxiliary 0 state)) :float state)))

(def matrix
  [[0, 1, 0, 0, 1, 0, 1, 0, 0, 1], [0, 1, 2, 0, 1, 0, 2, 1, 0, 1], [0, 1, 2, 3, 1, 0, 3, 3, 0, 1], [0, 0, 1, 2, 1, 2, 3, 2, 0, 0], [0, 0, 0, 1, 1, 2, 1, 2, 3, 0], [0, 0, 0, 0, 1, 1, 1, 2, 2, 3], [0, 0, 0, 0, 0, 1, 1, 1, 1, 2], [0, 0, 0, 0, 0, 0, 1, 1, 1, 1], [0, 0, 0, 0, 0, 0, 0, 1, 1, 1], [0, 0, 0, 0, 0, 0, 0, 0, 1, 1]]
  )


(def fitness-cases
  (for [r (range 10) c (range 10)]
    [r c (nth (nth matrix r) c)]))

(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [[r c target] fitness-cases]
                        (let [state (run-push program 
                                              (push-item c :auxiliary 
                                                         (push-item r :auxiliary 
                                                                    (make-push-state))))
                              ;top-int (top-item :integer state)
                              top-float (top-item :float state)]
                          (if (number? top-float) ;(number? top-int)
                            ;(Math/abs (float (- top-int target)))
                            (Math/abs (- top-float target))
                            1000)))))
  :atom-generators (concat [(fn [] (lrand-int 10))
                            'r
                            'c
                            'fr
                            'fc
                            ;'integer_div
                            ;'integer_mult
                            ;'integer_add
                            ;'integer_sub
                            ]
                           ;(registered-for-type :integer :include-randoms false)
                           ;(registered-for-type :float :include-randoms false)
                           ;(registered-for-type :exec :include-randoms false)
                           ;(registered-for-type :boolean :include-randoms false)
                           '(
                              ;code_nthcdr
                              ;exec_y
                              ;code_insert
                              ;zip_insert_left_fromexec
                              ;fr
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
                              float_rot
                              ;boolean_eq
                              ;string_pop
                              float_yank
                              ;code_subst
                              ;string_take
                              ;zip_fromcode
                              ;code_overlap
                              ;code_yankdup
                              float_sin
                              ;zip_end?
                              ;code_fromziprights
                              integer_yank
                              ;boolean_yank
                              ;zip_insert_right_fromexec
                              ;zip_insert_child_fromexec
                              float_frominteger
                              ;zip_swap
                              ;boolean_fromfloat
                              ;zip_yankdup
                              ;string_eq
                              ;zip_insert_left_fromcode
                              ;exec_eq
                              ;zip_fromexec
                              integer_dup
                              float_cos
                              float_stackdepth
                              ;zip_stackdepth
                              ;zip_right
                              ;code_null
                              integer_yankdup
                              ;zip_append_child_fromexec
                              ;code_pop
                              ;code_swap
                              ;r
                              float_swap
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
                              float_div
                              ;code_container
                              ;code_if
                              ;exec_when
                              ;exec_do*times
                              ;code_extract
                              integer_shove
                              ;float_rand
                              ;boolean_rand
                              float_shove
                              ;code_wrap
                              integer_mult
                              ;zip_prev
                              integer_stackdepth
                              float_sub
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
                              float_flush
                              ;zip_eq
                              ;code_map
                              float_yankdup
                              ;code_rand
                              ;exec_fromziplefts
                              ;integer_gt
                              integer_max
                              ;c
                              ;code_atom
                              ;exec_fromzipchildren
                              ;zip_remove
                              ;zip_flush
                              ;float_fromboolean
                              ;code_contains
                              ;boolean_not
                              integer_fromfloat
                              ;code_list
                              ;fc
                              ;boolean_or
                              ;code_do*range
                              ;exec_k
                              ;integer_fromboolean
                              ;code_fromzipnode
                              ;boolean_frominteger
                              ;float_gt
                              ;exec_yank
                              float_add
                              float_tan
                              ;code_eq
                              ;string_yank
                              ;zip_pop
                              ;code_fromzipchildren
                              float_mult
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
                              float_max
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
                              float_pop
                              ;boolean_yankdup
                              ;zip_insert_child_fromcode
                              ;float_eq
                              ;code_frominteger
                              ;zip_leftmost
                              float_min
                              ;boolean_dup
                              ;string_concat
                              ;code_do*count
                              ;code_car
                              ;zip_insert_right_fromcode
                              ;exec_dup
                              integer_min
                              ;zip_next
                              ;string_shove
                              ;code_position
                              float_dup
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
                              float_mod
                              ))
  :use-single-thread false
  :max-points 150
  :mutation-probability 0.4
  :mutation-max-points 20
  :crossover-probability 0.4
  :simplification-probability 0.1
  :tournament-size 7
  :report-simplifications 100
  :final-report-simplifications 1000
  :reproduction-simplifications 10
  :trivial-geography-radius 50
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
