(ns clojush.instructions.char
  (:use [clojush pushstate globals]
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
             (push-item (Character/isLetter item)
                        :boolean)))
      state)))

(define-registered
  char_isdigit
  (fn [state]
    (if (not (empty? (:char state)))
      (let [item (stack-ref :char 0 state)]
        (->> (pop-item :char state)
             (push-item (Character/isDigit item)
                        :boolean)))
      state)))

(define-registered
  char_iswhitespace
  (fn [state]
    (if (not (empty? (:char state)))
      (let [item (stack-ref :char 0 state)]
        (->> (pop-item :char state)
             (push-item (or (= item \newline)
                            (= item \space)
                            (= item \tab))
                        :boolean)))
      state)))
