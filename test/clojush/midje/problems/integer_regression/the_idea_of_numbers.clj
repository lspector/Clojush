; To run these tests with autotest use:
;
;    lein midje :autotest test
;
; This runs everything in the test sub-directory but
; _doesn't_ run all the stuff in src, which midje tries
; to run by default, which breaks the world.

(ns clojush.midje.problems.integer-regression.the-idea-of-numbers
  (:use clojure.test
        clojush.pushstate
        clojush.interpreter
        midje.sweet
        clojush.problems.integer-regression.the-idea-of-numbers))

(facts "birthday-polynomial works as expected"
  (birthday-polynomial 0 0 0 0) => 0
  (birthday-polynomial 1 1 1 1) => 3  ;; 1 + 1*1 + 1*1*1
  (birthday-polynomial 2 3 4 5) => 31  ;; 3 + 4*2 + 5*2*2
  (birthday-polynomial 0 1988 9 12) => 1988
  )

;; checking error function
;;

(fact "missing-numbers-error-function responds with the number of cases indicated by the argument"
  (count ((missing-numbers-error-function 5) '())) => 5 ;; (tests on 0,1,2,3,4)
  )

(fact "missing-numbers-error-function produces the expected penalties when no answer is returned"
  ((missing-numbers-error-function 2) '()) => (just 1000000000 1000000000) ;; empty program
  )

(fact "missing-numbers-error-function produces the expected scores"
  ((missing-numbers-error-function 3) '(0)) => (just 1964 1984 2026) ;; 1964+0; 1964+9+11; 1964+18+36
  ((missing-numbers-error-function 3) '(1000)) => (just 964 984 1026) ;; 1000 closer!
  ((missing-numbers-error-function 3) 
    '(1964 9 in1 integer_mult 11 in1 in1 integer_mult integer_mult integer_add integer_add)) => (just 0 0 0) ;; the right answer
  )

;; check atom-generators
(fact "atom-generators have no numbers"
  missing-numbers-atom-generators => (has not-any? integer?))

(fact "atom-generators does include the input (always good to check)"
  missing-numbers-atom-generators => (contains ['in1]))
