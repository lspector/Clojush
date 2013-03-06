(ns clojush.experimental.calc
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.random]
        [clojush.util]
        [clojush.instructions.tag]
        [clojure.math.numeric-tower]))

;; Lee Spector, 20130101

;; buttons to float answer (on stack) + boolean error signal (via instruction)
;; if no error must have answer on float stack and NOT have signal error
;; if error must signal error

;; modeled on the Staples SPL-110
(def buttons
  [:percent       :square-root  :off         :on-clear
   :memory-recall :memory-minus :memory-plus :clear-entry
   :seven         :eight        :nine        :divided-by
   :four          :five         :six         :times
   :one           :two          :three       :minus
   :zero          :point        :equals      :plus])

(def button-entrypoints
  (zipmap buttons (iterate #(+ % 100) 0)))

; button-entrypoints

(define-registered 
  signal_error 
  (fn [state] (push-item :error :auxiliary state)))

(def calc-tests
  ;; [inputs answer error]
  ;; answer doesn't matter if error is true
  ;; if no error, answer must be correct on float stack and true cannot be top boolean
  [;[[:one :divided-by :zero :equals] 0.0 true]
   [[:one] 1.0 false]
   ;[[:one :plus] 1.0 false]
   ;[[:one :plus :one] 1.0 false]
   ;[[:one :plus :one :equals] 2.0 false]
   ;[[:two :plus :two :equals] 4.0 false]
   ;[[:nine :times :nine :equals] 81.0 false]
   ;[[:three :divided-by :four :equals] 0.75 false]
   ;[[:one :two :three :four :five :plus :six :seven :eight :nine :zero :equals] 80235.0 false]
   ;[[:one :point :two :three :times :four :point :five :six :equals] 5.6088 false]
   ;[[:on-clear] 0.0 false]
   ;[[:nine :nine :nine :on-clear] 0.0 false]
   ;[[:one :point :two] 1.2 false]
   ;[[:one :point :two :point :three :point :four] 1.234 false]
   ;[[:one :point :two :plus :three :point :four :equals] 4.6 false]
   ;[[:one :point :two :times :three :point :four :equals] 4.08 false]
   ;[[:one :point :two :divided-by :two :equals] 0.6 false]
   [[:two] 2.0 false]
   [[:three] 3.0 false]
   [[:four] 4.0 false]
   [[:five] 5.0 false]
   [[:six] 6.0 false]
   [[:seven] 7.0 false]
   [[:eight] 8.0 false]
   [[:nine] 9.0 false]
   [[:zero] 0.0 false]
   [[:one :one] 11.0 false]
   [[:one :one :one] 111.0 false]
   [[:one :one :one :one] 1111.0 false]
   [[:one :one :one :one :one] 11111.0 false]
   [[:one :two] 12.0 false]
   [[:three :four :five] 345.0 false]
   [[:six :seven :eight :nine] 6789.0 false]
   ;[[:two :two :plus :two :two :equals] 44.0 false]
   ])
   
(defn calc-errors
  [program]
  ;; run the program once 
  ;; make an initialized push state with the resulting tag space and 0.0
  ;; then, for each fitness case:
  ;;   start with the initialized push state
  ;;   retrieve and execute all of the entry points from the pressed buttons
  ;;   determine error from float and boolean stacks
  (let [correct 0
        incorrect 1
        first-run-result (run-push program (make-push-state))
        initialized-push-state (push-item 
                                 0.0 
                                 :float 
                                 (assoc (make-push-state) :tag (:tag first-run-result)))
        err-modcount-pairs (doall (for [t calc-tests]
                                    (loop [push-state initialized-push-state
                                           buttons (first t)]
                                      (if (empty? buttons)
                                        [(if (nth t 2) 
                                           ;; should signal error, via the auxiliary stack
                                           (if (= :error (top-item :auxiliary push-state)) correct incorrect)
                                           ;; shouldn't signal error
                                           (if (= :error (top-item :auxiliary push-state))
                                             incorrect ;; but did
                                             (if (= (top-item :float push-state) (nth t 1))
                                               correct ;; answer is correct
                                               (let [top (top-item :float push-state)
                                                     target (nth t 1)]
                                                 (if (not (number? top))
                                                   incorrect ;; no number
                                                   ;; else return error scaled to [0, 1]
                                                   (/ (Math/abs (- top target))
                                                      (+ (Math/abs top) (Math/abs target))))))))
                                         (count (:tag push-state))]
                                        (recur (let [the-tag (get button-entrypoints (first buttons))]
                                                 (run-push (second (closest-association the-tag push-state))
                                                           push-state))
                                               (rest buttons))))))]
    ;(vec (cons (* 0.0000001 (reduce + (map second err-modcount-pairs)))
    ;           (map first err-modcount-pairs)))
    (vec (map first err-modcount-pairs))))

;; a little test -- wire each key to just push the right digit
;(calc-errors '(tag_exec_2000 0.0 tag_exec_1600 1.0 tag_exec_1700 2.0
;                             tag_exec_1800 3.0 tag_exec_1200 4.0
;                             tag_exec_1300 5.0 tag_exec_1400 6.0
;                             tag_exec_800 7.0 tag_exec_900 8.0
;                             tag_exec_1000 9.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def argmap
  {:error-function calc-errors
    :atom-generators (concat
                       '(signal_error)
                       ;(list (fn [] (- (lrand-int 21) 10))
                       ;      (fn [] (- (lrand 21) 10)))
                       [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]
                       (repeat 1 (tag-instruction-erc [:exec]))
                       (repeat 1 (tag-instruction-erc [:float :boolean]))
                       ;(repeat 1 (tag-instruction-erc [:float]))
                       (repeat 1 (tagged-instruction-erc))
                       ;(repeat 20 (return-tag-instruction-erc [:float :boolean :exec] 10000))
                       '(boolean_and
                          boolean_dup
                          boolean_eq
                          ;boolean_flush
                          boolean_fromfloat
                          ;boolean_frominteger
                          boolean_not
                          boolean_or
                          boolean_pop
                          ;boolean_rand
                          boolean_rot
                          ;boolean_shove
                          ;boolean_stackdepth
                          boolean_swap
                          ;boolean_yank
                          ;boolean_yankdup
                          ;code_append
                          ;code_atom
                          ;code_car
                          ;code_cdr
                          ;code_cons
                          ;code_container
                          ;code_contains
                          ;code_do
                          ;code_do*
                          ;code_do*count
                          ;code_do*range
                          ;code_do*times
                          ;code_dup
                          ;code_eq
                          ;code_extract
                          ;code_flush
                          ;code_fromboolean
                          ;code_fromfloat
                          ;code_frominteger
                          ;code_fromzipchildren
                          ;code_fromziplefts
                          ;code_fromzipnode
                          ;code_fromziprights
                          ;code_fromziproot
                          ;code_if
                          ;code_insert
                          ;code_length
                          ;code_list
                          ;code_map
                          ;code_member
                          ;code_noop
                          ;code_nth
                          ;code_nthcdr
                          ;code_null
                          ;code_pop
                          ;code_position
                          ;code_quote
                          ;code_rand
                          ;code_rot
                          ;code_shove
                          ;code_size
                          ;code_stackdepth
                          ;code_subst
                          ;code_swap
                          ;code_wrap
                          ;code_yank
                          ;code_yankdup
                          environment_begin
                          environment_end
                          environment_new
                          ;exec_do*count
                          ;exec_do*range
                          ;exec_do*times
                          exec_dup
                          exec_eq
                          ;exec_flush
                          ;exec_fromzipchildren
                          ;exec_fromziplefts
                          ;exec_fromzipnode
                          ;exec_fromziprights
                          ;exec_fromziproot
                          exec_if
                          exec_k
                          exec_noop
                          exec_pop
                          exec_rot
                          exec_s
                          ;exec_shove
                          ;exec_stackdepth
                          exec_swap
                          exec_when
                          exec_y
                          ;exec_yank
                          ;exec_yankdup
                          float_add
                          ;float_cos
                          float_div
                          float_dup
                          float_eq
                          ;float_flush
                          float_fromboolean
                          ;float_frominteger
                          float_gt
                          float_lt
                          ;float_max
                          ;float_min
                          ;;float_mod
                          float_mult
                          float_pop
                          ;float_rand
                          float_rot
                          ;float_shove
                          ;float_sin
                          ;float_stackdepth
                          float_sub
                          float_swap
                          ;float_tan
                          ;float_yank
                          ;float_yankdup
                          ;integer_add
                          ;integer_div
                          ;integer_dup
                          ;integer_eq
                          ;integer_flush
                          ;integer_fromboolean
                          ;integer_fromfloat
                          ;integer_gt
                          ;integer_lt
                          ;integer_max
                          ;integer_min
                          ;integer_mod
                          ;integer_mult
                          ;integer_pop
                          ;integer_rand
                          ;integer_rot
                          ;integer_shove
                          ;integer_stackdepth
                          ;integer_sub
                          ;integer_swap
                          ;integer_yank
                          ;integer_yankdup
                          return_boolean_pop
                          ;return_code_pop
                          return_exec_pop
                          return_float_pop
                          return_fromboolean
                          ;return_fromcode
                          return_fromexec
                          return_fromfloat
                          ;return_frominteger
                          ;return_fromstring
                          ;return_fromzip
                          ;return_integer_pop
                          ;return_string_pop
                          ;return_zip_pop
                          ;string_atoi
                          ;string_concat
                          ;string_contained
                          ;string_dup
                          ;string_eq
                          ;string_flush
                          ;string_length
                          ;string_parse_to_chars
                          ;string_pop
                          ;string_rand
                          ;string_reverse
                          ;string_rot
                          ;string_shove
                          ;string_stackdepth
                          ;string_swap
                          ;string_take
                          ;string_yank
                          ;string_yankdup
                          ;zip_append_child_fromcode
                          ;zip_append_child_fromexec
                          ;zip_branch?
                          ;zip_down
                          ;zip_dup
                          ;zip_end?
                          ;zip_eq
                          ;zip_flush
                          ;zip_fromcode
                          ;zip_fromexec
                          ;zip_insert_child_fromcode
                          ;zip_insert_child_fromexec
                          ;zip_insert_left_fromcode
                          ;zip_insert_left_fromexec
                          ;zip_insert_right_fromcode
                          ;zip_insert_right_fromexec
                          ;zip_left
                          ;zip_leftmost
                          ;zip_next
                          ;zip_pop
                          ;zip_prev
                          ;zip_remove
                          ;zip_replace_fromcode
                          ;zip_replace_fromexec
                          ;zip_right
                          ;zip_rightmost
                          ;zip_rot
                          ;zip_shove
                          ;zip_stackdepth
                          ;zip_swap
                          ;zip_up
                          ;zip_yank
                          ;zip_yankdup
                          )
                       )
   :use-single-thread false
   :use-lexicase-selection true
   :use-historically-assessed-hardness true ;; just to print them!
   ;:decimation-ratio 0.01
   ;:tournament-size 1
   :population-size 1000
   :max-generations 10001
   :evalpush-limit 1000
   :tag-limit 10000
   :max-points 1000
   :max-points-in-initial-program 25
   ;:parent-reversion-probability 0.9
   :mutation-probability 0
   :crossover-probability 0
   :simplification-probability 0
   :reproduction-simplifications 10
   :ultra-probability 1
   :ultra-alternation-rate 0.01
   :ultra-alignment-deviation 100
   :ultra-mutation-rate 0.01
   :deletion-mutation-probability 0
   :parentheses-addition-mutation-probability 0
   :tagging-mutation-probability 0
   :tag-branch-mutation-probability 0
   :tag-branch-mutation-type-instruction-pairs [[:boolean 'boolean_eq]
                                                [:float 'float_eq]
                                                [:float 'float_lt]
                                                [:float 'float_gt]]
   ;:pop-when-tagging false
   :report-simplifications 10
  })
