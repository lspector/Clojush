(ns string_tests
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

; Test string-rand
(println (run-push '(string-rand string-rand string-rand) (make-push-state)  false false))