(ns clojush.instructions.random-instructions
  (:use [clojush.pushstate]
        [clojush.random]
        [clojush.globals])
  (:require [clojure.math.numeric-tower :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random instructions

(define-registered
  boolean_rand
  (fn [state]
    (push-item (lrand-nth [true false]) :boolean state)))

(define-registered
  integer_rand
  (fn [state]
    (push-item (+' (lrand-int (+ 1 (- max-random-integer min-random-integer)))
                   min-random-integer)
               :integer
               state)))

(define-registered
  float_rand
  (fn [state]
    (push-item (+' (lrand (- max-random-float min-random-float))
                   min-random-float)
               :float
               state)))

(define-registered
  code_rand
  (fn [state]
    (if (not (empty? (:integer state)))
      (if (empty? @global-atom-generators)
        (binding [*out* *err*]
	         (println "code_rand: global-atom-generators is empty.")
	         state)
        (push-item (random-code (max 1
                                     (math/abs (mod (stack-ref :integer 0 state)
                                                    max-points-in-random-expressions)))
                                @global-atom-generators)
                   :code
                   (pop-item :integer state)))
      state)))

(define-registered
  string_rand
  (fn [state]
    (push-item
      (apply str (repeatedly
                   (+' min-random-string-length
                       (lrand-int (- max-random-string-length
                                     min-random-string-length)))
                   (fn [] (lrand-nth (concat ["\n" "\t"] (map (comp str char) (range 32 127)))))))
      :string
      state)))
