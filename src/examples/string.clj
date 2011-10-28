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
