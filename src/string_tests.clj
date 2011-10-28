(ns string_tests
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

; Test string-rand
;(println (run-push '(string_rand string_rand string_rand) (make-push-state)  false false))



; Test strings inserted into code
(println (run-push '("this" "is" "a" "string" 2 5 integer_add string_rand) (make-push-state)  false false))
