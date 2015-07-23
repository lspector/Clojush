; To run these tests with autotest use:
;
;    lein midje :autotest test
;
; This runs everything in the test sub-directory but
; _doesn't_ run all the stuff in src, which midje tries
; to run by default, which breaks the world.

(ns clojush.test.wiring-test
  (:use clojure.test      ;; No harm in retaining this
        midje.sweet
        ))

(deftest wiring
  (testing "Can we run tests?"
    (is (= (+ 2 3) 5))))

(fact "addition works"
  (+ 2 3) => 5)
