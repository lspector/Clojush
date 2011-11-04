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
                                      (if (>= unique-chars 8)
                                        0
                                        (- 8 unique-chars)))))))
        	 :atom-generators (list (fn [] (apply str (repeatedly (+ 1 (lrand-int 8))
                                                               #(rand-nth (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                                                               "abcdefghijklmnopqrstuvwxyz"
                                                                               "1234567890"))))))
                                   ;'string_rand)
                                   	 :tournament-size 5)
                                 


; Add string_concat to this example, and make the number of unique characters higher. Maybe only use
; upper case, and try to evolve a string with all 26 letters.



; New GP problem: Take the input string, remove the last 2 characters, and then concat this result with itself.
; The fitness will be the number of non-matching characters in the resulting string. For example,
; desired result of "abcde" would be "abcabc", and a string of "abcabcrrr" would have an error of 3, for
; 3 too many characters, and the string "aaaaaa" would have error of 4, since it gets 2 of the characters right.