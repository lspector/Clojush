(ns clojush.instructions.boolean
  (:use [clojush.pushstate]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for Booleans

(define-registered 
  boolean_and
  (fn [state]
    (if (not (empty? (rest (:boolean state))))
      (push-item (and (stack-ref :boolean 0 state)
                      (stack-ref :boolean 1 state))
                 :boolean
                 (pop-item :boolean (pop-item :boolean state)))
      state)))

(define-registered 
  boolean_or
  (fn [state]
    (if (not (empty? (rest (:boolean state))))
      (push-item (or (stack-ref :boolean 0 state)
                     (stack-ref :boolean 1 state))
                 :boolean
                 (pop-item :boolean (pop-item :boolean state)))
      state)))

(define-registered
  boolean_not
  (fn 
    [state]
    (if (not (empty? (:boolean state)))
      (push-item (not (stack-ref :boolean 0 state))
                 :boolean
                 (pop-item :boolean state))
      state)))

(define-registered 
  boolean_frominteger
  (fn [state]
    (if (not (empty? (:integer state)))
      (push-item (not (zero? (stack-ref :integer 0 state)))
                 :boolean
                 (pop-item :integer state))
      state)))

(define-registered 
  boolean_fromfloat
  (fn [state]
    (if (not (empty? (:float state)))
      (push-item (not (zero? (stack-ref :float 0 state)))
                 :boolean
                 (pop-item :float state))
      state)))
