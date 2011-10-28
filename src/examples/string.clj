(ns examples.string
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

; Tries to get a string with the most unique characters. If there are at least 8, succeeds.
(pushgp :error-function (fn [program]
                          (list (let [final-state (run-push program (make-push-state))
                                top-string (top-item :string final-state)]
                            (if (not (string? top-string))
                              1000
                              (let [unique-chars (count (distinct top-string))]
                                (if (> unique-chars 8)
                                  0
                                  (- 8 unique-chars)))))))
        	 :atom-generators (list 'string_rand)
        	 :tournament-size 5)
