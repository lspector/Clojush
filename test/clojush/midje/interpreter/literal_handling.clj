; To run these tests with autotest use:
;
;    lein midje :autotest test
;
; This runs everything in the test sub-directory but
; _doesn't_ run all the stuff in src, which midje tries
; to run by default, which breaks the world.

(ns clojush.midje.interpreter.literal-handling
  (:use clojure.test
        midje.sweet
        clojush.interpreter
        clojush.pushstate))

(fact "Evaluating a null instruction returns the same state"
  (execute-instruction nil :test-state) => :test-state)

(fact "Evaluating an integer constant as an instruction adds that value to the integer stack"
      (let [test-state (make-push-state)
            value 8]
        (:integer (execute-instruction value (make-push-state))) => (list value)))
