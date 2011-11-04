(ns string_tests
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

; Test string-rand
;(println (run-push '(string_rand string_rand string_rand) (make-push-state)  false false))



; Test strings inserted into code
#_(println (run-push '(string_rand "these" "are" "strings" string_concat)
                   (make-push-state)  false false))


#_(println (run-push '("abcde" "HelloWorld" 7 string_take)
                   (make-push-state)))

#_(println (run-push '("d2" string_length)
                   (make-push-state)))


; This is a solution to the string GP problem
(println (run-push '(string_dup string_length 2 integer_sub string_take string_dup string_concat)
                   (push-item "abcde" :string (make-push-state))))