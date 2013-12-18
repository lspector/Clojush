(ns clojush.examples.wc-tester
  (:use clojush.pushgp.pushgp
        clojush.examples.wc
        [clojush pushstate interpreter random globals]
        clojush.instructions.tag
        [clojush evaluate individual]
        clojure.math.numeric-tower))

(reset! global-atom-generators wc-atom-generators)
(reset! global-evalpush-limit 2000)
(reset! global-max-points 1000)

;;;;
;evolved solution
(def log3 '(((string_readline string_readline exec_do*count integer_rot integer_mult (file_EOF file_EOF string_readline output_wordcount string_readline string_readline tag_integer_790 string_readline string_take string_readline string_readline string_stackdepth exec_dup string_swap string_concat string_concat string_concat string_concat string_concat string_concat boolean_and boolean_swap string_concat boolean_swap string_dup string_length string_reverse output_charcount integer_div string_split output_linecount string_split output_linecount integer_max output_linecount (string_yankdup integer_div) string_stackdepth output_wordcount output_linecount integer_div))) string_stackdepth output_wordcount))

(def log4 '(exec_stackdepth exec_do*count string_readline string_stackdepth output_linecount exec_do*times string_concat string_yankdup string_length output_charcount output_wordcount output_wordcount string_readline string_split string_split string_stackdepth output_wordcount exec_do*range))

(def log6 '(((((tagged_91 boolean_and integer_swap integer_add integer_add integer_add boolean_and integer_swap integer_add string_readline integer_yankdup string_readline output_wordcount exec_swap string_readline string_readline integer_dec exec_yankdup string_readline exec_s string_readline string_readline)) 70 exec_if exec_shove string_readline exec_shove string_readline string_readline exec_shove string_readline string_readline integer_mod string_readline string_readline output_linecount output_linecount string_stackdepth integer_dup integer_dup exec_swap output_linecount integer_rot output_linecount) integer_shove integer_min file_begin exec_stackdepth exec_do*count string_concat string_length output_charcount string_readline string_readline string_readline string_readline string_readline string_readline exec_yankdup exec_do*range exec_stackdepth) exec_do*times string_concat string_split file_begin string_split 88 string_stackdepth output_wordcount))

(def log13 '((string_readline exec_if string_readline string_yankdup string_readline string_readline exec_when boolean_or string_readline string_readline string_readline string_readline string_stackdepth integer_swap integer_max 5 string_concat output_charcount string_concat string_concat string_concat string_concat string_concat string_concat string_readchar string_concat integer_rot string_dup string_length output_charcount string_concat output_linecount exec_do*count) exec_dup exec_s exec_s string_readline boolean_swap string_split exec_dup exec_shove exec_dup (output_wordcount output_wordcount exec_dup string_stackdepth)))

(def log14 '((string_readline string_readline string_readline (string_readline integer_min (string_readline string_readline ((string_readline string_readline exec_s string_stackdepth string_concat exec_do*range string_readline) boolean_or (string_concat string_concat exec_when) string_concat exec_when string_concat exec_when boolean_dup string_concat exec_when) string_concat exec_when string_dup string_length output_charcount string_stackdepth output_linecount tagged_607 exec_rot output_linecount integer_max string_split) string_stackdepth) string_stackdepth string_stackdepth output_wordcount string_length)))

(def log16 '(((exec_do*count exec_do*count string_readline string_take (string_readline string_readline (string_readline integer_min integer_mult integer_min boolean_or boolean_not string_readline boolean_dup) string_readline output_linecount exec_pop string_readline output_linecount string_stackdepth)) string_readline integer_rot tagged_823 tagged_823 exec_stackdepth string_stackdepth boolean_dup output_linecount string_stackdepth string_stackdepth boolean_dup output_linecount exec_dup string_concat string_concat 25 string_concat boolean_not string_concat string_concat exec_dup integer_add string_dup string_length output_charcount) string_concat string_split string_stackdepth output_wordcount string_split integer_yankdup exec_shove))

(def log18 '((tag_string_305 string_readline string_readline output_charcount string_readline string_readline string_readline string_readline string_yank string_readline string_readline output_linecount exec_do*times exec_do*range boolean_stackdepth integer_sub output_linecount string_stackdepth boolean_swap output_linecount string_take output_wordcount string_concat) (output_charcount exec_dup string_concat string_concat exec_s exec_dup exec_dup string_concat string_concat integer_swap string_concat boolean_swap file_begin integer_sub integer_sub string_dup exec_rot integer_mod string_split string_length file_begin string_stackdepth exec_swap output_charcount output_wordcount)))

(def sol27 '(((((exec_stackdepth exec_do*count string_readline exec_do*count string_concat exec_do*count string_stackdepth) exec_eq output_charcount exec_do*count string_stackdepth ("\t" exec_shove string_length (exec_yankdup string_rot string_dup) string_rot) string_pop output_charcount string_split string_whitespace exec_rot string_stackdepth)) string_dup (integer_yank) exec_s output_wordcount boolean_stackdepth (exec_s) "\n" ((string_contained) ((file_begin) (string_stackdepth string_flush) (((string_shove (exec_do*times integer_yankdup (integer_add exec_s ((exec_s integer_rot) string_rot (integer_min tag_string_246 exec_when exec_shove string_readline) string_readline) string_readline) string_readline string_readline string_readline)) string_readline) string_readline))) string_stackdepth) output_linecount))

(defn test-evolved-program
  [prog data-domains]
  (let [result (evaluate-individual (make-individual :program prog)
                     (wc-error-function data-domains)
                     (new java.util.Random))
        errors (partition-all 3 (:errors result))]
    (map (fn [test-case error]
           (vector error test-case))
         (first (first data-domains))
         errors)))

(def test-data-domains
  [[(list (apply str (repeat 100 \newline))
          (apply str (repeat 100 \space))
          (apply str (repeat 100 \tab))
          (apply str (repeat 100 \A))
          (apply str (take 100 (cycle (list \A \newline))))
          (apply str (take 100 (cycle (list \B \newline \newline))))
          (apply str (take 100 (cycle (list \C \D \newline))))
          (apply str (take 100 (cycle (list \E \space))))
          (apply str (take 100 (cycle (list \F \tab))))
          (apply str (take 100 (cycle (list \x \newline \y \space))))
          (apply str (take 100 (cycle (list \space \newline))))) 20 0]])

(test-evolved-program sol27 test-data-domains)

#_(filter #(not= (first %) '(0 0 0))
        (test-evolved-program sol27))
