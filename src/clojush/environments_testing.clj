(ns clojush.environments-testing
  (:use [clojush.evaluate]
        [clojush.individual]
        [clojush.globals]
        [clojush.interpreter]
        [clojush.pushstate]
        [clojush.util]
        [clojush.pushgp.pushgp]
        ))


(run-push '(5 2 4.3 2.3 6 environment_push (integer_add float_mult) integer_sub)
          (push-item "hi there" :auxiliary
                     (push-item "hi there" :string
                                (make-push-state))))