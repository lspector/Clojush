(ns clojush.instructions.char
  (:use [clojush pushstate globals]
        ;[clojure.string :only [split trim]]
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for characters

(define-registered
  char_allfromstring
  (fn [state]
    (if (empty? (:string state))
      state
      (loop [char-list (reverse (top-item :string state))
             loop-state (pop-item :string state)]
        (if (empty? char-list)
          loop-state
          (recur (rest char-list)
                 (push-item (first char-list) :char loop-state)))))))

(define-registered
  char_frominteger
  (fn [state]
    (if (not (empty? (:integer state)))
      (let [item (stack-ref :integer 0 state)]
        (->> (pop-item :integer state)
             (push-item (char (mod item 128)) :char)))
      state)))

(define-registered
  char_fromfloat
  (fn [state]
    (if (not (empty? (:float state)))
      (let [item (stack-ref :float 0 state)]
        (->> (pop-item :float state)
             (push-item (char (mod (long item) 128)) :char)))
      state)))

(define-registered
  char_isletter
  (fn [state]
    (if (not (empty? (:char state)))
      (let [item (stack-ref :char 0 state)]
        (->> (pop-item :char state)
             (push-item (or (<= (int \A) (int item) (int \Z))
                            (<= (int \a) (int item) (int \z)))
                        :boolean)))
      state)))

;; Instructions needed (o = instruction done and tested
;;                      x = instruction done and in pushstate):

;;;; CHARACTER
;x integer_fromchar
;x char_frominteger
;x float_fromchar
;x char_fromfloat
;x print_char
;x char_isletter
; char_isdigit
; char_iswhitespace
; char_rand
; char_atoi(?)

;;;; STRING
; string_frominteger, string_fromfloat, string_fromboolean, string_fromchar
; string_replacesubstring
; string_rest, string_butlast

;;;; STRING AND CHAR
; string_charat, string_indexof (char), string_containschar
; string_replaceonechar, string_replaceallchars, string_removechar, string_conjchar
; string_first, string_last, string_nth
; [rename string_contained (?)]

;;;; OTHER
;x print_newline


