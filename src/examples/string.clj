(ns examples.string
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

; Tries to get a string with the most unique characters. If there are at least goal, succeeds.
(pushgp :error-function (fn [program]
                          (let [goal 26]
                            (list (let [final-state (run-push program (make-push-state))
                                        top-string (top-item :string final-state)]
                                    (if (not (string? top-string))
                                      1000
                                      (let [unique-chars (count (distinct top-string))]
                                        (if (>= unique-chars goal)
                                          0
                                          (- goal unique-chars))))))))
        	 :atom-generators (list (fn [] (apply str (repeatedly (+ 1 (lrand-int 5))
                                                               #(rand-nth (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))))
                                 'string_concat
                                 'string_dup
                                 'string_rot)
        	 :tournament-size 5)


; New GP problem: Take the input string, remove the last 2 characters, and then concat this result with itself.
; The fitness will be the number of non-matching characters in the resulting string. For example,
; desired result of "abcde" would be "abcabc", and a string of "abcabcrrr" would have an error of 3, for
; 3 too many characters, and the string "aaaaaa" would have error of 4, since it gets 2 of the characters right.