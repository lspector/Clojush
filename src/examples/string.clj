(ns examples.string
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

; Tries to get a string with the most unique characters. If there are at least goal, succeeds.
#_(pushgp :error-function (fn [program]
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

(defn string-difference
  "Returns the difference in the strings, based on character position."
  [s1 s2]
  (+ (reduce + (map #(if (= %1 %2)
                       0
                       1)
                    s1
                    s2))
     (Math/abs (- (count s1) (count s2)))))

(defn string-remove-char
  "Returns s with first instrance of c removed, if c is in s. Otherwise, returns nil"
  [s c]
  (if (empty? s)
    nil
    (if (= (first s) c)
      (clojure.contrib.string/drop 1 s)
      (let [remain (string-remove-char (clojure.contrib.string/drop 1 s) c)]
        (if (nil? remain)
          nil
          (str (first s) remain))))))

(defn string-char-counts-difference
  "Returns the summed length of the strings, minus 2 for each character that the strings have in common.
   For example, string-char-counts-difference of 'abcd' and 'pabja' would be 5."
  [s1 s2]
  (if (empty? s1)
    (count s2)
    (let [first-char (first s1)
          remove-from-s2 (string-remove-char s2 first-char)]
      (if (nil? remove-from-s2)
        (inc (string-char-counts-difference (clojure.contrib.string/drop 1 s1) s2))
        (string-char-counts-difference (clojure.contrib.string/drop 1 s1) remove-from-s2)))))
      

(define-registered in 
                   (fn [state] (push-item (stack-ref :auxiliary 0 state) :string state)))

(pushgp :error-function (fn [program]
                          (doall
                            (for [input '("abcde"
                                           ""
                                           "E"
                                           "Hi"
                                           "Tom"
                                           "leprechaun"
                                           "zoomzoomzoom"
                                           "qwertyuiopasd"
                                           "GallopTrotCanter"
                                           "Quinona")]
                              (let [final-state (run-push program 
                                                          (push-item input :auxiliary 
                                                                     (push-item input :string 
                                                                                (make-push-state))))
                                    top-string (top-item :string final-state)
                                    desired-output (let [short (clojure.contrib.string/butlast 2 input)]
                                                     (str short short))]
                                (if (not (string? top-string))
                                  1000
                                  (+ (string-difference top-string desired-output)
                                     (string-char-counts-difference top-string desired-output)))))))
        :atom-generators (list 'in
                               'string_length
                               'string_take
                               'string_concat
                               'string_stackdepth
                               'string_swap
                               'string_dup
                               'integer_add
                               'integer_sub
                               'integer_dup
                               'integer_swap
                               'integer_stackdepth
                               (fn [] (rand-int 10))
                               (fn [] (apply str (repeatedly (+ 1 (lrand-int 9))
                                                             #(rand-nth (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                                                             "abcdefghijklmnopqrstuvwxyz"
                                                                             "0123456789"))))))
        :population-size 500
        :max-generations 200
        :tournament-size 5)



; This is a solution to the string GP problem
#_(println (run-push '(string_dup string_length 2 integer_sub string_take string_dup string_concat)
                     (push-item "abcde" :string (make-push-state))))
        