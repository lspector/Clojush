
;============== NOTE NOTE NOTE =================
; This file has note been updated for Clojush 2.0, and will not work
; for example, genetic operators haven't been updated
;============== NOTE NOTE NOTE =================

(ns clojush.problems.software.calc
  (:use [clojush.pushgp.pushgp]
        [clojush.args]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojush.random]
        [clojush.util]
        [clojush.instructions.tag]
        [clojure.math.numeric-tower]        
        [clojush.globals]
        [clojush.experimental.tagged-code-macros]
        ;[incanter.stats :as stats]
        ))

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
  
(def digit-keys 
  [:zero :one :two :three :four :five :six :seven :eight :nine])
  
(def digit-key-map (into {} (map vector digit-keys (iterate inc 0))))

(define-registered 
  signal_error 
  (fn [state] (push-item :error :auxiliary state)))
  
(def digit-entry-behavior
  {:old-tests (vec (for [k (range 10)] 
                     [[(nth digit-keys k)] 
                      (float k) 
                      false]))
   :new-tests []
   :augment-fn (fn [b n] b)})
  
(def digit-entry-pair-behavior
  {:old-tests []
   :new-tests []
   :augment-fn (fn [b n] ; behavior max-num-new-tests -> behavior
                 (let [tests (concat (:old-tests b) (:new-tests b))
                       num-tests (count tests)]
                   {:old-tests tests
                    :new-tests (loop [new-tests []
                                      remaining n]
                                 (if (or (zero? remaining)
                                         (= (+ num-tests (count new-tests)) 100))
                                   new-tests
                                   (recur (conj new-tests
                                                (loop []
                                                  (let [k1 (lrand-nth (range 10))
                                                        k2 (lrand-nth (range 10))
                                                        t [[(nth digit-keys k1) (nth digit-keys k2)] 
                                                           (float (+ (* 10 k1) k2)) 
                                                           false]]
                                                    (if (some #{t} (concat tests new-tests))
                                                      (recur)
                                                      t))))
                                          (dec remaining))))
                    :augment-fn (:augment-fn b)}))})
  
(def single-digit-math-behavior
  {:old-tests []
   :new-tests []
   :augment-fn (fn [b n] ; behavior max-num-new-tests -> behavior
                 (let [tests (concat (:old-tests b) (:new-tests b))
                       num-tests (count tests)]
                   {:old-tests tests
                    :new-tests (loop [new-tests []
                                      remaining n]
                                 (if (or (zero? remaining)
                                         (= (+ num-tests (count new-tests)) (- (* 10 10 4) 10))) ;; subtract out the divisions by zero
                                   new-tests
                                   (recur (conj new-tests
                                                (loop []
                                                  (let [[arg1-key arg1-val] (lrand-nth (vec digit-key-map))
                                                        [arg2-key arg2-val] (lrand-nth (vec digit-key-map))
                                                        safe-div (fn [n d] (if (zero? d) 0.0 (/ n d)))
                                                        [op-key op-fn] (lrand-nth (vec {:plus +, :minus -, :times *, :divided-by safe-div}))
                                                        t [[arg1-key op-key arg2-key :equals] 
                                                           (float (op-fn arg1-val arg2-val)) 
                                                           false]]
                                                    (if (or (and (= op-key :divided-by) (zero? arg2-val))
                                                            (some #{t} (concat tests new-tests)))
                                                      (recur)
                                                      t))))
                                          (dec remaining))))
                    :augment-fn (:augment-fn b)}))})

;;; THE FOLLOWING TEST FUNCTIONS ARE OBSOLETE AND HAVE TO BE TURNED INTO "BEHAVIORS" AS ABOVE

(defn single-digit-incomplete-math-tests [n]
  (take n (lshuffle
            (apply concat
              (for [[arg1-key arg1-val] digit-key-map
                    [arg2-key arg2-val] digit-key-map
                    [op-key op-fn] {:plus +, :minus -, :times *, :divided-by /}]
                [[[arg1-key op-key arg2-key] ;; leave off the =
                  (float arg2-val) 
                  false]
                 [[arg1-key op-key] ;; leave off the second arg and the =
                  (float arg1-val) 
                  false]])))))
    
;(single-digit-incomplete-math-tests 4)
      
(defn single-digit-chained-math-tests [n]
  (take n (lshuffle
            (for [[arg1-key arg1-val] digit-key-map
                  [arg2-key arg2-val] digit-key-map
                  [op1-key op1-fn] {:plus +, :minus -, :times *, :divided-by /}
                  [arg3-key arg3-val] digit-key-map
                  [op2-key op2-fn] {:plus +, :minus -, :times *, :divided-by /}
                  :when (not (or (and (= op1-key :divided-by) (zero? arg2-val))
                                 (and (= op2-key :divided-by) (zero? arg3-val))))]
              (do #_(println arg1-key arg1-val arg2-key arg2-val op1-key 
                       op1-fn arg3-key arg3-val op2-key op2-fn)
              [[arg1-key op1-key arg2-key op2-key arg3-key :equals] 
               (float (op2-fn (op1-fn arg1-val arg2-val) arg3-val))
               false])))))

;(single-digit-chained-math-tests 4)
  
(defn division-by-zero-tests [n]
  (take n (lshuffle 
            (for [[arg1-key arg1-val] digit-key-map
                  [arg2-key arg2-val] {:zero 0}]
              [[arg1-key :divided-by arg2-key :equals] 
               0.0 
               true]))))
    
;(division-by-zero-tests 4)
    
(defn double-digit-float-entry-tests [n]
  (take n (lshuffle 
            (for [[arg1-key arg1-val] digit-key-map
                  [arg2-key arg2-val] digit-key-map]
              [[arg1-key :point arg2-key] 
               (float (+ arg1-val (* 0.1 arg2-val))) 
               false]))))
  
;(double-digit-float-entry-tests 4)
  
;(def calc-tests (atom [digit-entry-tests
;                       digit-entry-pair-tests
;                       single-digit-math-tests]))

(def calc-behaviors (atom [digit-entry-behavior
                           digit-entry-pair-behavior
                           single-digit-math-behavior]))
  
(defn calc-tests []
  (vec (concat (apply concat (map :old-tests @calc-behaviors))
               (apply concat (map :new-tests @calc-behaviors)))))

(defn reset-calc-behaviors! []
  (swap! calc-behaviors
         (fn [behaviors]
           (doall (map #((:augment-fn %) % 10)
                       behaviors)))))

(defn calc-report-with-reset!
  [best population generation error-function report-simplifications]
  (when (<= (:total-error best) min-number-magnitude)
    (println "Resetting cases.")
    (reset-calc-behaviors!)
    (println "New number of tests:" (count (calc-tests)))
    (println "New tests:" (calc-tests))
    (println "New behaviors:" @calc-behaviors))
  best
  )

(println "Number of tests:" (count (calc-tests)))
(println "Behaviors:" @calc-behaviors)

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
        first-run-result (run-push program
                                   (tag-before-entry-points (make-push-state)))
        initialized-push-state (push-item 
                                 0.0 
                                 :float 
                                 (assoc (make-push-state) :tag (:tag first-run-result)))
        test-errors (doall (for [t (calc-tests)]
                             (loop [push-state initialized-push-state
                                    buttons (first t)]
                               (if (empty? buttons)
                                 (let [top (top-item :float push-state)
                                       target (nth t 1)]
                                   (if (not (number? top))
                                     [incorrect 
                                      ;incorrect
                                      ]
                                     (if ;(== top target)
                                       (< (Math/abs (- top target)) 0.00001)
                                       ;; correct
                                       [correct 
                                        ;correct
                                        ]
                                       ;incorrect
                                       [(Math/abs (- top target)) 
                                        ;1.0
                                        ;(float (levenshtein-distance (format "%+20.10f" top) (format "%+20.10f" target)))
                                        ]))
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
    (let [errs (vec (apply concat test-errors))
          ;num-errs (count errs)
          ]
      errs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def argmap
  {:error-function calc-errors
   :atom-generators (concat
                      ;'(signal_error)
                      [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]
                      [(tag-instruction-erc [:exec])]
                      ;(for [t (vals button-entrypoints)]
                      ;  (fn [] (symbol (str "tag_exec_" (str t)))))
                      [(tagged-instruction-erc)]
                      ;; allow code_append tag macros (taking 2 arguments and returning 1 result)
                      ;[(tagged-code-macro-erc 'code_append @global-tag-limit 2 1)]
                      ;(repeat 46 'code_noop)
                      '(;;;;boolean_and
                         ;;;;boolean_dup
                         ;;;;boolean_eq
                         ;boolean_flush
                         ;;;;boolean_fromfloat
                         ;boolean_frominteger
                         ;;;;boolean_not
                         ;;;;boolean_or
                         ;;;;boolean_pop
                         ;boolean_rand
                         ;;;;boolean_rot
                         ;boolean_shove
                         ;boolean_stackdepth
                         ;;;;boolean_swap
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
                         ;;;;exec_dup
                         ;;;;exec_eq
                         ;exec_flush
                         ;exec_fromzipchildren
                         ;exec_fromziplefts
                         ;exec_fromzipnode
                         ;exec_fromziprights
                         ;exec_fromziproot
                         ;;;;exec_if
                         ;;;;exec_k
                         ;;;;exec_noop
                         ;exec_pop
                         ;;;;exec_rot
                         ;;;;exec_s
                         ;exec_shove
                         ;exec_stackdepth
                         ;;;;exec_swap
                         ;;;;exec_when
                         ;;;;exec_y
                         ;exec_yank
                         ;exec_yankdup
                         float_add
                         ;;;;float_cos
                         float_div
                         float_dup
                         ;;;;float_eq
                         ;float_flush
                         ;;;;float_fromboolean
                         ;float_frominteger
                         ;;;;float_gt
                         ;;;;float_lt
                         ;;;;float_max
                         ;;;;float_min
                         ;;;;float_mod
                         float_mult
                         float_pop
                         ;float_rand
                         float_rot
                         ;float_shove
                         ;;;;float_sin
                         ;float_stackdepth
                         float_sub
                         float_swap
                         ;;;;float_tan
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
   ;:trivial-geography-radius 500
   ;:parent-selection :elitegroup-lexicase
   ;:total-error-method :hah ;; just to print them!
   ;:decimation-ratio 0.01
   ;:tournament-size 1
   :population-size 1000 ;200 ;50
   :max-generations 100000
   :evalpush-limit 3000
   :tag-limit 10000
   :max-points 12000
   :max-genome-size-in-initial-program 500 ;;100
   :mutation-probability 0.0
   :crossover-probability 0.0
   :simplification-probability 0
   :reproduction-probability 0
   :reproduction-simplifications 10
   :ultra-probability 1.0
   :ultra-alternation-rate 0.001
   :ultra-alignment-deviation 100
   :ultra-mutation-rate 0.05
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
   :generate-bushy-random-code true
   :ultra-mutates-to-parentheses-frequently true
   :use-ultra-no-paren-mutation false
   :ultra-pads-with-empties false
   :problem-specific-report calc-report-with-reset!
   :reuse-errors false
   :error-threshold -1
   :print-error-frequencies-by-case true
  })
  
;; needed to validate

;(load-push-argmap argmap)
;(reset-globals)
