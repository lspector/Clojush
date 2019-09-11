(ns clojush.mod-metrics6
  (:use [clojush.util]
        [clojush.simplification]
        ;[clojush.ns]
        ;[clojure.math.numeric-tower]
        [clojush.problems.software.double-letters-c :as dl]
        [clojush.individual :as individual]
        ))

;(use-clojush)
;; Need to return reuse, repetition, size of program, size of program after simplification (steps=5000)  
(defn evaluate-individual-for-modmetrics
 "Return reuse and repetition metrics as lists. The length of each list is equal to the number of test cases."
 [ind prog argmapp]
 (let [error-func (get argmapp :error-function)
        modified-ind (error-func ind :train)]
   (list (reduce + (:reuse-info modified-ind)) (reduce + (:repetition-info modified-ind)) (count-points (:program ind)) (count-points (:program (auto-simplify-from-program prog error-func 100 false 100))) )
  ))


(defn main-func
  [prog]
  (let  [;prog '(tagged_728 string_removechar exec_noop integer_rot print_string string_yankdup string_length integer_dec in1 integer_mult integer_dup_items boolean_invert_second_then_and exec_noop print_string integer_div string_removechar integer_empty exec_rot (string_dup) (boolean_dup) (string_parse_to_chars integer_eq boolean_pop char_flush integer_yank integer_dec boolean_swap char_flush boolean_not string_swap "pNc" integer_stackdepth) string_dup_times string_eq exec_s (boolean_dup_items char_isletter string_removechar string_take integer_dup_times) () (in1 tag_integer_39 char_pop) string_length integer_rot integer_fromstring char_stackdepth string_setchar "d" exec_do*range (\( string_rest) char_eq string_replacefirstchar string_dup boolean_yankdup integer_fromstring string_dup char_pop integer_shove exec_eq boolean_yankdup exec_y (integer_rot) boolean_dup_times char_dup_times boolean_dup exec_empty in1 exec_do*count (string_rest) in1 string_length boolean_frominteger integer_min integer_fromchar boolean_yank boolean_flush exec_when (string_split exec_do*count (string_replacefirstchar string_stackdepth integer_stackdepth) char_isdigit in1 char_allfromstring exec_flush) integer_add " w=0n " string_stackdepth string_containschar string_dup_times char_swap exec_shove (exec_noop exec_flush print_newline exec_dup_items char_iswhitespace char_isletter tagged_922) boolean_and string_dup boolean_empty string_occurrencesofchar string_yankdup string_frominteger " Wij-P _  ',kwI" exec_do*range (string_flush char_yank integer_dup_times) integer_shove string_conjchar string_nth print_integer string_rest string_fromboolean boolean_yank char_iswhitespace integer_dec string_dup_times in1 string_yank exec_do*range (integer_inc) integer_div exec_k () (print_char char_yankdup print_boolean \C) string_shove integer_gt exec_yankdup char_yankdup boolean_invert_second_then_and string_pop exec_eq exec_yankdup exec_y (exec_dup_items exec_do*range (char_iswhitespace string_first) char_empty) exec_yank exec_k (char_frominteger boolean_dup string_emptystring \newline boolean_or exec_dup (string_reverse boolean_yankdup boolean_invert_second_then_and) boolean_frominteger exec_do*times ()) (exec_s (integer_fromstring string_yankdup string_indexofchar integer_pop exec_rot (string_setchar char_yankdup string_fromchar char_rot char_stackdepth exec_flush integer_yank print_integer char_allfromstring integer_shove char_isletter string_stackdepth exec_dup (boolean_dup_times integer_stackdepth integer_yankdup boolean_dup string_shove) integer_add print_integer) (exec_dup (integer_empty exec_string_iterate (string_yankdup char_stackdepth integer_dup_times tag_exec_539 (char_rot tagged_330)))) ()) () ()))
         myind (individual/make-individual :program prog)]
    (prn (evaluate-individual-for-modmetrics myind prog dl/argmap))))



