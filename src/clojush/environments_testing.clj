(ns clojush.environments-testing
  (:use [clojush.evaluate]
        [clojush.individual]
        [clojush.globals]
        [clojush.interpreter]
        [clojush.pushstate]
        [clojush.util]
        [clojush.pushgp.pushgp]
        ))


(run-push '(5 2 29999 4.3 2.3
              environment_new (integer_add float_mult return_frominteger)
              integer_sub)
          (push-item "hi" :auxiliary
                     (push-item "hi" :string
                                (make-push-state)))
          true)

(run-push '(3 environment_begin 4 return_frominteger environment_end 5 6)
          (make-push-state)
          true)