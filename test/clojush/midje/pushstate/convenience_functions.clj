; To run these tests with autotest use:
;
;    lein midje :autotest test
;
; This runs everything in the test sub-directory but
; _doesn't_ run all the stuff in src, which midje tries
; to run by default, which breaks the world.

(ns clojush.midje.pushstate.convenience-functions
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util]
        clojure.test
        midje.sweet
        ))

(fact "push-state-now! creates a push-state with all available stacks"
  (keys (push-state-now!)) => (just clojush.globals/push-types :in-any-order)
  )

(fact "push-state-now! creates a empty stacks if none are passed in"
  (concat (vals (push-state-now!))) => (has every? nil?)
  )

(facts "push-state-now! sets a named stack to the values passed in as arguments"
  (:integer (push-state-now! :integer '(1 2 3))) => (just 1 2 3)
  (:boolean (push-state-now! :boolean '(false true))) => (just false true )
  (:code (push-state-now! :code '( 122 false integer_add))) => (contains   #{ 122 false 'integer_add})
  (:vector_integer (push-state-now! :vector_integer '([1 2 3] [4 5 6]))) =>
    (just [1 2 3] [4 5 6])
  )

(fact "passing in weird keys just drops them into the pushstate"
  (:foo (push-state-now! :foo '(:a :b :c))) => (just :a :b :c) ;; a victimless crime?
  )

(fact "the result of push-state-now! can be used to run code and stuff"
  (:integer (run-push '(integer_add integer_add) (push-state-now! :integer '(1 2 3 4)))) => (just 6 4) 
  )

