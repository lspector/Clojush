; To run these tests with autotest use:
;
;    lein midje :autotest test
;
; This runs everything in the test sub-directory but
; _doesn't_ run all the stuff in src, which midje tries
; to run by default, which breaks the world.

(ns clojush.midje.problems.tozier.winkler01
  (:use clojure.test
        midje.sweet
        clojush.problems.tozier.winkler01))

(facts "count-digits returns the number of digits in a number"
  (count-digits 999) => 3
  (count-digits -123456789) => 9
  (count-digits -1.23) => 3  ;; you really shouldn't do this
  (count-digits "foo") => 0) ;; or this

(facts "proportion-not-01 returns the fraction of (all) digits not 0 or 1 in a number"
  (proportion-not-01 999) => 1
  (proportion-not-01 111234) => 1/2
  (proportion-not-01 -1.23) => 2/3) 
;;(proportion-not-01 "foo") => not defined
;;   (would expect it to raise a divide by zero error)


;; how does one test an error function like 'winkler-error-function?
;;
;; it should:
;; - check the value for an empty program
;; - check the value for a program with "known" result
;; - check the value for a program that gives no result