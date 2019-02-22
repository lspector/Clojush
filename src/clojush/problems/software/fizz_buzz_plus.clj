;; fizz_buzz_plus.clj
;; the silly interview question, because why not?
;; Bill Tozier (Feb 9, 2018; updated Feb 1, 2019)

(ns clojush.problems.software.fizz-buzz-plus
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

;;;;;;;;;;;;
;; Given an :integer input argument, and two :integer constants i and j, produce
;; a :string which includes the first argument integer at the start. If the
;; first argument is divisible by either i or j, add a space, then append the
;; :string "fizz" if the input is divisible by i, and (:string) "buzz" if it
;; is divisible by j. In cases where it is divisible by both, produce
;; "fizzbuzz". Otherwise produce a string containing only the integer, and no
;; extra space.


(defn divisible_string
  "given a numerator and denominator, return the result_string if the numerator is evenly divisible by the denominator, or an empty string otherwise"
  [numerator denominator result_string]
  (if (zero? (mod numerator denominator))
    result_string
    ""))


(defn fizz-buzz-plus-number-string
  "given a number, and two factors, return a single string concatenating the number plus its fizz & buzz strings"
  [n i j]
  (let [f (divisible_string n i "fizz")
        b (divisible_string n j "buzz")
        fb (str f b)]
    (if (empty? fb)
      (str n)
      (str n " " f b)
      )))


(defn fizz-buzz-plus-training-case
  "given a numerator number and two factors, return a complete FizzBuzzPlus case as a vector, including the expected string in the last position"
  [numerator i j]
  [numerator i j (fizz-buzz-plus-number-string numerator i j)]
  )


; Helper function for error function
(defn fizz-buzz-plus-training-cases
  "Takes a starting point and ending point (integers), and a set of prospective factors. For each case, it picks two different items from the collection factors, with uniform probability, and returns a training case as a vector [numerator i j expected_string]. Note that i and j are not sorted."
  [startpoint endpoint factors]
  (let [numerators (range startpoint endpoint)]
    (map
      #(let [[i j] (take 2 (shuffle factors))]
        (fizz-buzz-plus-training-case % i j))
      numerators
      )))



(defn fizz-buzz-plus-error
  "Based on some undocumented stuff I found in another problem, and I have no real idea why things are named these things or what said things do. Except `errors`, which uses the Levenshtein distance between the expected string, and the string obtained from the top of the stack at the end of the run."
  [individual cases]
  (let [behaviors (vec (for [a-case cases]
                         (->> (make-push-state)
                              (push-item , (first a-case) :input)
                              (push-item , (nth a-case 1) :input)
                              (push-item , (nth a-case 2) :input)
                              (run-push , (:program individual))
                              (top-item , :string))))
        errors (mapv (fn [behavior case]
                       (if (string? behavior)
                        (levenshtein-distance behavior (last case))
                        1000))
                     behaviors
                     cases)]
    (assoc individual
      :behaviors behaviors
      :errors errors)))




(def fizz-buzz-plus-atom-generators
  "Collection of items which will be used to construct random programs: the strings 'fizz' and 'buzz' are given, since we don't really want to evolve them from scratch (though we could). Also some integer constants, boolean constants, the input and various standard Push instructions."
  (concat (list
            "fizz"
            "buzz"
            "fizzbuzz"
            \space
            ;;; end constants
            (fn [] (- (lrand-int 21) 10))    ;; Integer ERC [-10,10]
            (fn [] (- (lrand-int 201) 100))  ;; Integer ERC [-100,100]
            (fn [] (lrand-nth (list true false)))
            ;;; end ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :exec :char :string :code])
          ))



(def all-cases
  (fizz-buzz-plus-training-cases 2000 2100 [2 3 4 5 7]))

(println all-cases)

(def argmap
  {:error-function (fn [individual]
                      (fizz-buzz-plus-error
                        individual
                        all-cases
                        ))
   :atom-generators fizz-buzz-plus-atom-generators
   :max-points 1000
   :max-genome-size-in-initial-program 1000
   :evalpush-limit 1000
   :population-size 500
   :max-generations 5000
   :parent-selection :lexicase
   :meta-error-categories [:novelty]
   :individuals-for-novelty-archive-per-generation 1
   :genetic-operator-probabilities {:alternation 0.4
                                    :uniform-mutation 0.6
                                    }
   :alternation-rate 0.05
   :uniform-mutation-rate 0.05
   :report-simplifications 0
   :final-report-simplifications 5000
   })

;; some evolved examples:
;;

;; program: (in3 in2 integer_mod exec_yankdup string_stackdepth (string_eq exec_string_iterate in3 in3 in2 integer_mod char_yank string_stackdepth in3 in1 integer_mod \space integer_mod exec_do*range (in3 string_frominteger) string_conjchar "fizz" string_occurrencesofchar string_concat string_yankdup "buzz" string_concat string_yankdup))
;;
;; program: (string_conjchar \space \space string_conjchar \space string_conjchar \space \space string_conjchar in3 in1 integer_mod string_conjchar string_conjchar integer_mod string_conjchar in1 char_isletter boolean_dup_items exec_do*range (in3 in2 in2 string_replace string_replace string_dup_times string_dup_times string_replace integer_mod integer_min code_do*count string_dup_times exec_do*range (in1 integer_mod code_do*count exec_do*range (in3 in2 string_replace string_replace string_dup_times string_replace integer_mod integer_min integer_mod string_replace string_dup_times string_replace string_replace string_dup_times string_replace integer_min code_nth string_dup_times integer_min string_dup_times integer_shove integer_min code_nth string_dup_times integer_min string_dup_times integer_shove integer_min string_dup_times exec_do*range (in1 in3 in2 string_replace integer_empty integer_mod integer_min exec_do*range (code_position char_empty integer_min integer_shove in3 in2 integer_lt char_flush boolean_invert_second_then_and) in2 string_indexofchar integer_min in2 string_indexofchar integer_min integer_shove code_noop integer_shove integer_shove in1 code_noop string_reverse code_noop integer_shove integer_shove integer_shove in1 code_noop string_empty exec_dup_items exec_swap (code_if string_concat code_if code_eq code_wrap boolean_xor boolean_dup integer_mod string_empty string_empty integer_max) (boolean_xor (code_dup_times boolean_xor exec_when (string_concat boolean_dup integer_mod string_empty integer_sub) string_fromchar integer_eq in3 string_dup string_fromchar string_nth string_frominteger exec_while (exec_dup_times exec_flush boolean_pop) exec_swap (integer_flush code_dup_times) (char_dup_times) exec_swap (code_fromboolean string_conjchar boolean_swap) (boolean_pop) string_nth string_frominteger exec_shove exec_swap (string_conjchar code_do* char_stackdepth code_stackdepth code_do*range) (\space)) code_do*times code_yank exec_if ("fizz" string_replacechar boolean_invert_second_then_and exec_when () string_replacechar string_replace string_occurrencesofchar string_replace string_replace string_occurrencesofchar string_replace string_replace exec_when () char_rot boolean_dup string_replacechar string_replace boolean_invert_second_then_and exec_when () string_replacechar boolean_invert_second_then_and exec_when string_replacechar string_replace char_rot boolean_dup string_replacechar string_replace string_replacechar boolean_invert_second_then_and exec_when () string_replacechar string_replace string_occurrencesofchar string_replace string_replacechar code_append in2 boolean_xor) (in2 code_append in2 code_position code_contains boolean_xor) string_stackdepth "buzQ" string_pop code_position) boolean_xor) code_do* exec_when () code_position boolean_xor) code_car boolean_xor) "buzz" string_concat))
;;
;; program: (in3 string_frominteger in3 \space in2 integer_mod string_conjchar exec_yankdup "fizz" true string_fromchar code_extract exec_swap (in3 3 integer_dup_times) code_container exec_yankdup "buzz" in3 in3 in1 integer_mod in3 in2 integer_mod in3 in1 integer_mod integer_dup_times exec_yankdup exec_swap string_split (boolean_shove string_nth string_concat string_dup_items))
;;
;; program: (in3 in1 integer_mod exec_do*times char_frominteger in3 in2 integer_mod exec_do*times char_dup in2 integer_mod char_isdigit exec_do*times char_dup integer_fromchar in3 integer_eq \space exec_shove (in3 boolean_not string_frominteger) string_conjchar in3 boolean_dup_times char_empty "fizz" string_removechar string_indexofchar string_concat exec_while integer_min "buzz" string_concat)


;; NOT QUITE
;;
;; almost-solutions
;; Best program: (in1 in2 char_dup_times boolean_rot in3 in2 integer_mod integer_mod integer_stackdepth integer_div integer_dec integer_rot string_containschar \space in3 13 integer_yankdup code_dup integer_mod exec_yankdup in3 string_frominteger string_replacechar exec_yank code_do* boolean_rot code_yankdup code_yank boolean_empty boolean_dup_items exec_flush exec_eq code_dup_times char_yank 14 string_rest integer_yankdup integer_mod exec_yankdup in3 string_frominteger code_do* char_empty char_shove code_wrap code_cons boolean_pop exec_dup (exec_y (exec_yankdup char_yank char_empty code_nthcdr) exec_eq \space) string_conjchar exec_yankdup string_replacechar 11 exec_yankdup integer_pop integer_empty boolean_stackdepth string_empty integer_gte integer_dec char_iswhitespace code_pop string_fromboolean string_empty code_nth boolean_xor integer_mult exec_y (string_conjchar exec_yankdup integer_pop integer_empty boolean_stackdepth string_empty char_eq code_dup_times code_if exec_do*count (integer_gte integer_dec char_iswhitespace integer_dup "fizz" boolean_flush "bu`L" code_extract char_allfromstring exec_string_iterate (char_dup_times exec_when () exec_do*while (string_conjchar code_append)))))
;;
;; Best program: (string_fromchar char_dup_times 15 integer_swap code_empty false char_pop string_split code_map code_quote () string_replacefirst integer_inc string_split code_map string_split string_contains string_contains 15 string_contains code_dup code_quote () string_nth integer_inc string_split code_map string_split string_fromboolean integer_dup_items char_dup_times 15 integer_inc 15 integer_dup_items integer_dup_times 17 integer_inc 15 integer_inc char_dup_times 15 integer_dup 15 integer_gte "buzz" 15 char_isletter 15 integer_inc string_length code_swap code_cdr boolean_not 15 integer_inc string_length code_swap code_cdr string_shove 15 integer_inc string_length string_containschar code_cdr in3 in3 string_split string_butlast char_empty code_fromboolean string_replacefirstchar code_cdr integer_fromstring string_concat char_flush code_swap char_yank in2 integer_mod integer_rot code_length boolean_or string_yank string_pop code_map string_fromboolean in3 string_fromchar exec_dup_items code_map char_empty in1 code_length integer_fromchar integer_fromchar char_flush string_frominteger integer_gt -3 in3 string_flush in1 string_reverse string_frominteger integer_gt -3 in3 string_flush in1 string_reverse integer_mod exec_s () (\space) ("buzz" "buzz" char_empty code_shove false) exec_empty integer_div in2 code_cons in3 string_frominteger char_yank code_if integer_shove exec_dup_times (char_empty exec_do*range (string_replacefirstchar integer_div integer_lt code_car) exec_while (code_if) exec_do*range (string_conjchar integer_lt code_empty "fizz" code_yank code_length code_cons string_concat exec_y (char_pop integer_shove) boolean_invert_first_then_and) \space code_insert code_eq char_iswhitespace exec_do*range () code_swap) string_conjchar char_dup "buzz" code_if string_concat)