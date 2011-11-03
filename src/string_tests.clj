(ns string_tests
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

; Test string-rand
;(println (run-push '(string_rand string_rand string_rand) (make-push-state)  false false))



; Test strings inserted into code
(println (run-push '(string_rand "these" "are" "strings" string_concat)
                   (make-push-state)  false false))
