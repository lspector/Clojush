; To run these tests with autotest use:
;
;    lein midje :autotest test
;
; This runs everything in the test sub-directory but
; _doesn't_ run all the stuff in src, which midje tries
; to run by default, which breaks the world.

(ns clojush.midje.problems.tozier.winkler01
  (:use clojure.test
        clojush.pushstate
        clojush.interpreter
        midje.sweet
        clojush.problems.tozier.winkler01))

;; check count-digits
;;

(facts "count-digits returns the number of digits in a number"
  (count-digits 999) => 3
  (count-digits -123456789) => 9
  (count-digits -1.23) => 3  ;; you really shouldn't do this
  (count-digits "foo") => 0) ;; or this

;; check proportion-not-01
;;

(facts "proportion-not-01 returns the fraction of (all) digits not 0 or 1 in a number"
  (proportion-not-01 999) => 1
  (proportion-not-01 111234) => 1/2
  (proportion-not-01 1001221330) => 4/10
  (proportion-not-01 1100110011) => 0
  (proportion-not-01 -1.23) => 2/3 
  (proportion-not-01 "foo") => (throws Exception #"Divide by zero"))

;; check kill-trailing-zeros
(facts "kill-trailing-zeros returns an integer with all trailing zeros trimmed off"
  (kill-trailing-zeros 999) => 999
  (kill-trailing-zeros 110000) => 11
  (kill-trailing-zeros 100020003000) => 100020003
  (kill-trailing-zeros 1.23000) => 1.23) ;; yes, the interpreter already does this


(facts "prime-factors returns a cons containing the prime factors of the argument"
  (type (prime-factors 2)) => clojure.lang.Cons
  (prime-factors 7) => [7]
  (prime-factors 256) => [2 2 2 2 2 2 2 2]
  (sort (prime-factors 1000)) => [2 2 2 5 5 5]
  (sort (prime-factors 11010011011100010)) => [2 3 3 5 7 13 199 1511 4470811]
  )

(facts "prime-factors-as-sorted-vector should return the factors as a sorted vector"
  (type (prime-factors-as-sorted-vector 2)) => clojure.lang.PersistentVector
  (prime-factors-as-sorted-vector 1000) => [2 2 2 5 5 5]
  (prime-factors-as-sorted-vector 11010011011100010) => [2 3 3 5 7 13 199 1511 4470811]
)

(facts "prime-factors-as-sorted-vector returns 'reasonable' results for bad integer inputs"
  (prime-factors-as-sorted-vector -2) => [-1 2]
  (prime-factors-as-sorted-vector -21) => [-1 3 7]
  (prime-factors-as-sorted-vector 0 ) => [0]
  (prime-factors-as-sorted-vector -11010011011100010) => [-1 2 3 3 5 7 13 199 1511 4470811]
  )

;; checking the instruction integer_factors
;;

(fact "checking the new instruction is registered"
  (registered-for-stacks [:integer :vector_integer]) => (contains 'integer_factors))

;; some convenience functions
;;

(defn state-with-an-int [my-int] (push-item my-int :integer (make-push-state)))
(defn run-in-int-sandbox [program my-int] (run-push program (state-with-an-int my-int)))

(fact 
  "integer_factors returns a vector_integer"
  (vector? (first (:vector_integer (run-in-int-sandbox '(integer_factors) 7)))) => truthy
)

(facts
  "integer_factors returns a vector_integer containing the result of prime-factors-as-sorted-vector"
  (first (:vector_integer (run-in-int-sandbox '(integer_factors) 7))) => (prime-factors-as-sorted-vector 7)
  (first (:vector_integer (run-in-int-sandbox '(integer_factors) 88))) => (prime-factors-as-sorted-vector 88)
  (first (:vector_integer (run-in-int-sandbox '(integer_factors) 0))) => (prime-factors-as-sorted-vector 0)
  (first (:vector_integer (run-in-int-sandbox '(integer_factors) -1024))) => 
    (prime-factors-as-sorted-vector -1024)
)


;; checking winkler-error-function-01
;;

(fact "winkler-error-function-01 responds with the number of cases indicated by the argument"
  (count ((winkler-error-function-01 5) '())) => 4 ;; (tests on 1,2,3,4)
  )

(facts "winkler-error-function-01 counts 1s and 0s in the product of output * top integer"
  ((winkler-error-function-01 12) '(1))    => [0 1 1 1 1 1 1 1 1 0 0]  ;; 1, 10, and 11
  ((winkler-error-function-01 12) '(1000)) => [0 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 0 0] ;; 1000, 10000, 11000
  )

(fact "winkler-error-function-01 returns a score of 100 as a penalty for not answering"
  ((winkler-error-function-01 5) '(integer_pop)) => [100 100 100 100]  ;; no answer given
  )

(fact "winkler-error-function-01 returns does not leave input 'hint' on :integer stack"
  ((winkler-error-function-01 5) '()) => [100 100 100 100]  ;; should not have answers from :integer stack 
  )


;; checking winkler-error-function-02
;;

(fact "winkler-error-function-02 responds with the number of cases indicated by the argument"
  (count ((winkler-error-function-02 5) '())) => 4 ;; (tests on 1,2,3,4)
  )

(facts "winkler-error-function-02 counts 1s and 0s in the product of output * top integer"
  ((winkler-error-function-02 12) '(1))    => [0 1 1 1 1 1 1 1 1 0 0]  ;; 1, 10, and 11
  ((winkler-error-function-02 12) '(1000)) => [0 1 1 1 1 1 1 1 1 0 0 ] ;; 1000, 10000, 11000
  )

(fact "winkler-error-function-02 returns a score of 100 as a penalty for not answering"
  ((winkler-error-function-02 5) '(integer_pop)) => [100 100 100 100]  ;; no answer given
  )

(fact "winkler-error-function-02 returns does not leave input 'hint' on :integer stack"
  ((winkler-error-function-02 5) '()) => [100 100 100 100]  ;; should not have answers from :integer stack 
  )

(println (count (registered-for-stacks [:integer :boolean :string :char :exec :print])))