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

(def digit-entry-pair-tests
  (let [keys [:zero :one :two :three :four :five :six :seven :eight :nine]]
    (vec (for [k1 (range 10) k2 (range 10)] 
           [[(nth keys k1) (nth keys k2)] (float (+ (* 10 k1) k2)) false]))))

(def single-digit-math-tests
  (vec (for [[arg1-key arg1-val] {:zero 0, :three 3, :seven 7}
             [arg2-key arg2-val] {:two 2, :four 4, :nine 9}
             [op-key op-fn] {:plus +, :minus -, :times *, :divided-by /}]
         [[arg1-key op-key arg2-key :equals] (float (op-fn arg1-val arg2-val)) false])))

;single-digit-math-tests

(def single-digit-incomplete-math-tests
  (vec (apply concat
              (for [[arg1-key arg1-val] {:zero 0, :three 3, :seven 7}
                    [arg2-key arg2-val] {:two 2, :four 4, :nine 9}
                    [op-key op-fn] {:plus +, :minus -, :times *, :divided-by /}]
                [[[arg1-key op-key arg2-key] (float arg2-val) false]
                 [[arg1-key op-key] (float arg1-val) false]]))))
  
;single-digit-incomplete-math-tests
    
(def single-digit-chained-math-tests
  (vec (for [[arg1-key arg1-val] {:zero 0, :three 3, :seven 7}
             [arg2-key arg2-val] {:two 2, :four 4, :nine 9}
             [op1-key op1-fn] {:plus +, :minus -, :times *, :divided-by /}
             [arg3-key arg3-val] {:three 3, :five 5, :eight 8}
             [op2-key op2-fn] {:plus +, :minus -, :times *, :divided-by /}]
         [[arg1-key op1-key arg2-key op2-key arg3-key :equals] 
          (float (op2-fn (op1-fn arg1-val arg2-val) arg3-val))
          false])))

;single-digit-chained-math-tests

(def division-by-zero-tests
  (vec (for [[arg1-key arg1-val] {:zero 0, :three 3, :seven 7}
             [arg2-key arg2-val] {:zero 0}]
         [[arg1-key :divided-by arg2-key :equals] 0.0 true])))
  
;division-by-zero-tests
  
(def double-digit-float-entry-tests
  (vec (for [[arg1-key arg1-val] {:zero 0, :three 3, :seven 7}
             [arg2-key arg2-val] {:two 2, :four 4, :nine 9}]
         [[arg1-key :point arg2-key] (float (+ arg1-val (* 0.1 arg2-val))) false])))

;(double-digit-float-entry-tests)

(def calc-tests
  ;; [inputs answer error]
  ;; answer doesn't matter if error is true
  ;; if no error, answer must be correct on float stack and true cannot be top boolean
  (concat
    digit-entry-pair-tests
    single-digit-math-tests
    ;single-digit-incomplete-math-tests
    ;single-digit-chained-math-tests
    ;division-by-zero-tests
    ;double-digit-float-entry-tests
    ))

(println "Number of cases:" (count calc-tests))

(defn tag-before-entry-points
  [push-state]
  (let [tags (->> button-entrypoints
               (vec)
               (map second)
               (map dec)
               (map #(if (< % 0) 9999 %)))] ;; hardcoded for tag-limit of 10000
    (assoc push-state :tag (merge (sorted-map)
                                  (:tag push-state) 
                                  (zipmap tags (repeat ()))))))
   
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
        first-run-result (run-push program (tag-before-entry-points (make-push-state)))
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
    (vec (map first err-modcount-pairs))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def argmap
  {:error-function calc-errors
    :atom-generators (concat
                       '(signal_error)
                       ;(list (fn [] (- (lrand-int 21) 10))
                       ;      (fn [] (- (lrand 21) 10)))
                       [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]
                       ;(repeat 1 (tag-instruction-erc [:exec :float :boolean]))
                       (repeat 1 (tag-instruction-erc [:exec]))
                       ;(for [t (vals button-entrypoints)]
                       ;  (fn [] (symbol (str "tag_exec_" (str t)))))
                       ;[(fn []
                       ;   (symbol (str "tag_exec_" (str (lrand-nth (vals button-entrypoints))))))]
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
                          ;environment_begin
                          ;environment_end
                          ;environment_new
                          ;exec_do*count
                          ;exec_do*range
                          ;exec_do*times
                          ;exec_dup
                          ;exec_eq
                          ;exec_flush
                          ;exec_fromzipchildren
                          ;exec_fromziplefts
                          ;exec_fromzipnode
                          ;exec_fromziprights
                          ;exec_fromziproot
                          exec_if
                          ;exec_k
                          ;exec_noop
                          ;exec_pop
                          ;exec_rot
                          ;exec_s
                          ;exec_shove
                          ;exec_stackdepth
                          ;exec_swap
                          ;exec_when
                          ;exec_y
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
                          ;return_boolean_pop
                          ;return_code_pop
                          ;return_exec_pop
                          ;return_float_pop
                          ;return_fromboolean
                          ;return_fromcode
                          ;return_fromexec
                          ;return_fromfloat
                          ;return_frominteger
                          ;return_fromstring
                          ;return_tagspace
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
   ;:use-idiolexicase-selection true
   :use-historically-assessed-hardness true ;; just to print them!
   ;:decimation-ratio 0.01
   ;:tournament-size 1
   :population-size 200
   :max-generations 10001
   :evalpush-limit 200
   :tag-limit 10000
   :max-points 2000
   :max-points-in-initial-program 1000
   ;:parent-reversion-probability 0.9
   :mutation-probability 0
   :crossover-probability 0
   :simplification-probability 0
   :reproduction-simplifications 10
   :ultra-probability 1.0
   :ultra-alternation-rate 0.005
   :ultra-alignment-deviation 20
   :ultra-mutation-rate 0.005
   :deletion-mutation-probability 0
   :parentheses-addition-mutation-probability 0
   :tagging-mutation-probability 0
   :tag-branch-mutation-probability 0.0
   :tag-branch-mutation-type-instruction-pairs [[:boolean 'boolean_eq]
                                                [:float 'float_eq]
                                                [:float 'float_lt]
                                                [:float 'float_gt]]
   ;:pop-when-tagging false
   :report-simplifications 10
   :print-history false
  })
