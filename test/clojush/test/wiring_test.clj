(ns clojush.test.wiring_test
  (:use clojure.test      ;; No harm in retaining this
        ; midje.sweet
        ))

(deftest wiring
  (testing "Can we run tests?"
    (is (= (+ 2 3) 5))))
