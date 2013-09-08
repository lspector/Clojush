(ns clojush.experimental.calc
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.random]
        [clojush.util]
        [clojush.instructions.tag]
        [clojure.math.numeric-tower]
        ;[incanter.stats :as stats]
        ))

;;;;;;;;
(defn compute-next-row
  "computes the next row using the prev-row current-element and the other seq"
  [prev-row current-element other-seq pred]
  (reduce
    (fn [row [diagonal above other-element]]
      (let [update-val
	     (if (pred other-element current-element)
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
	       (inc (min diagonal above (peek row)))
	       )]
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
In information theory and computer science, the Levenshtein distance is a metric for measuring the amount of difference between two sequences. This is a functional implementation of the levenshtein edit
distance with as little mutability as possible.
 
Still maintains the O(n*m) guarantee.
"
  [a b & {p :predicate  :or {p =}}]
  (peek
    (reduce
      ;; we use a simple reduction to convert the previous row into the next-row  using the
      ;; compute-next-row which takes a current element, the previous-row computed so far
      ;; and the predicate to compare for equality.
      (fn [prev-row current-element]
	(compute-next-row prev-row current-element b p))
 
      ;; we need to initialize the prev-row with the edit distance between the various prefixes of
      ;; b and the empty string.
      (map #(identity %2) (cons nil b) (range)) 
      a)))
;;;;;;

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

(def digit-entry-tests
  (let [keys [:zero :one :two :three :four :five :six :seven :eight :nine]]
    (vec (for [k (range 10)] 
           [[(nth keys k)] (float k) false]))))

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
    digit-entry-tests
    digit-entry-pair-tests
    single-digit-math-tests
    ;single-digit-incomplete-math-tests
    ;single-digit-chained-math-tests
    ;division-by-zero-tests
    ;double-digit-float-entry-tests
    ))

(println "Number of tests:" (count calc-tests))

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
  ;;   determine errors from float and boolean stacks
  (let [correct 0.0
        incorrect 1000000000.0
        first-run-result (run-push program (tag-before-entry-points (make-push-state)))
        initialized-push-state (push-item 
                                   0.0 
                                   :float 
                                   (assoc (make-push-state) :tag (:tag first-run-result)))
        test-errors (doall (for [t calc-tests]
                             (loop [push-state initialized-push-state
                                    buttons (first t)]
                               (if (empty? buttons)
                                 (let [top (top-item :float push-state)
                                       target (nth t 1)]
                                   ;(println "TEST:" t)
                                   ;(println "FINAL STATE:" push-state)
                                   (if (not (number? top))
                                     [incorrect incorrect]
                                      (if (== top target)
                                        ;; correct
                                        [correct correct]
                                        ;incorrect
                                        [(Math/abs (- top target)) 
                                         (float (levenshtein-distance (str top) (str target)))]))
                                    ;;
                                    ;; error signal error
                                    #_(if (nth t 2)
                                      (if (= :error (top-item :auxiliary push-state)) 
                                        correct 
                                        incorrect)
                                      correct)
                                    ;])
                                    )
                                 (recur (let [the-tag (get button-entrypoints (first buttons))]
                                          (run-push (second (closest-association the-tag push-state))
                                                    push-state))
                                        (rest buttons))))))]
    ;(vec (apply concat test-errors))
    (let [all-errors (apply concat test-errors)]
      (conj (vec all-errors) (count (filter #(not (zero? %)) all-errors))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def argmap
  {:error-function calc-errors
    :atom-generators (concat
                       ;'(signal_error)
                       [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]
                       [(tag-instruction-erc [:exec])]
                       (for [t (vals button-entrypoints)]
                         (fn [] (symbol (str "tag_exec_" (str t)))))
                       [(tagged-instruction-erc)]
                       (repeat 46 'code_noop)
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
                          ;;code_fromboolean
                          ;code_fromfloat
                          ;code_frominteger
                          ;;code_fromzipchildren
                          ;;code_fromziplefts
                          ;;code_fromzipnode
                          ;;code_fromziprights
                          ;;code_fromziproot
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
                          ;;code_rand
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
                          ;exec_pop
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
                          float_cos
                          float_div
                          float_dup
                          float_eq
                          ;float_flush
                          float_fromboolean
                          ;float_frominteger
                          float_gt
                          float_lt
                          float_max
                          float_min
                          float_mod
                          float_mult
                          float_pop
                          ;float_rand
                          float_rot
                          ;float_shove
                          float_sin
                          ;float_stackdepth
                          float_sub
                          float_swap
                          float_tan
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
                          ;;string_rand
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
   :trivial-geography-radius 500
   ;:use-elitegroup-lexicase-selection true
   ;:use-historically-assessed-hardness true ;; just to print them!
   ;:decimation-ratio 0.01
   ;:tournament-size 1
   :population-size 10000 ;200 ;50
   :max-generations 10001
   :evalpush-limit 3000
   :tag-limit 10000
   :max-points 3000
   :max-points-in-initial-program 200 ;;100
   :parent-reversion-probability 0.0
   :mutation-probability 0
   :crossover-probability 0
   :simplification-probability 0
   :reproduction-simplifications 10
   :ultra-probability 1.0
   :ultra-alternation-rate 0.005
   :ultra-alignment-deviation 5
   :ultra-mutation-rate 0.005
   :deletion-mutation-probability 0
   :parentheses-addition-mutation-probability 0
   :tagging-mutation-probability 0
   :tag-branch-mutation-probability 0.0
   :tag-branch-mutation-type-instruction-pairs [[:boolean 'boolean_eq]
                                                [:float 'float_eq]
                                                [:float 'float_lt]
                                                [:float 'float_gt]]
   :pop-when-tagging true
   :report-simplifications 0
   :print-history false
  })
