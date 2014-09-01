(ns clojush.instructions.char
  (:use [clojush pushstate globals]
        ;[clojure.string :only [split trim]]
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for characters

(define-registered
  char_fromstring
  (fn [state]
    (if (empty? (:string state))
      state
      (loop [char-list (reverse (top-item :string state))
             loop-state (pop-item :string state)]
        (if (empty? char-list)
          loop-state
          (recur (rest char-list)
                 (push-item (first char-list) :char loop-state)))))))
