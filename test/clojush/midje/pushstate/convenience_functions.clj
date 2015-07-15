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
   
(fact "push-state-from-stacks creates a push-state with all available stacks"
  (keys (push-state-from-stacks)) => (just clojush.globals/push-types :in-any-order)
  )

(fact "push-state-from-stacks creates a empty stacks if none are passed in"
  (concat (vals (push-state-from-stacks))) => (has every? nil?)
  )

(facts "push-state-from-stacks sets a named stack to the values passed in as arguments"
  (:integer (push-state-from-stacks :integer '(1 2 3))) => (just 1 2 3)
  (:boolean (push-state-from-stacks :boolean '(false true))) => (just false true )
  (:code (push-state-from-stacks :code '( 122 false integer_add))) => (contains   #{ 122 false 'integer_add})
  (:vector_integer (push-state-from-stacks :vector_integer '([1 2 3] [4 5 6]))) =>
    (just [1 2 3] [4 5 6])
  )


(facts "the unspecified stacks are still empty"
  (:foo (push-state-from-stacks :foo '(:some :webgl :commands :here))) => (just :some :webgl :commands :here)
  (:integer (push-state-from-stacks :foo '(:some :webgl :commands :here))) => nil
  )

(def big-state (push-state-from-stacks :integer '(1 2) :boolean '(false) :char '(\f \w \i \w) :rational '(3/4 111/9)))

(facts "push-state-from-stacks works for multiple stacks"
  (:integer big-state) => (just 1 2)
  (:boolean big-state) => (just false)
  (:char big-state) => (just \f \w \i \w)
  (:rational big-state) => (just 3/4 111/9))

(fact "passing in weird keys just drops them into the pushstate"
  (:foo (push-state-from-stacks :foo '(:a :b :c))) => (just :a :b :c) ;; a victimless crime?
  )

(fact "the result of push-state-from-stacks can be used to run code and stuff"
  (:integer (run-push '(integer_add integer_add) (push-state-from-stacks :integer '(1 2 3 4)))) => (just 6 4) 
  )


;; if I want to merge whole new stacks into a pre-existing push-state,
;; I just want to make sure there is a simple way of doing that...

(def test-state (push-state-from-stacks :integer '(1 2)))

(fact "I don't need to write a special merge-push-state to overwrite the stack in a push-state"
  (:integer (merge test-state {:integer '(7 7 7)})) => (just 7 7 7)
  )

