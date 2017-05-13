;; given some number number n, return the nth prime

(ns clojush.problems.software.nth-prime
  (:require [clojush.pushstate :refer [push-item make-push-state top-item registered-for-stacks]]
            [clojush.instructions.tag :refer [tag-instruction-erc tagged-instruction-erc]]
            [clojush.interpreter :refer [run-push]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojush.random :refer [lrand-int]]))

(let [first-primes [2  3  5  7  11  13  17  19  23  29 31  37  41  43  47  53  59  61  67  71]]
  (defn nth-prime [n]
   (first-primes n)))

;; first prime is two
(assert (= 2 (nth-prime 0)))
;; 20th prime is 71
(assert (= 71 (nth-prime 19)))

(defn single-error-function [program n]
  (as-> (make-push-state) v
    (push-item n :input v)
    (run-push program v)
    (top-item :integer v)
    (if (number? v)
      (abs (- v (nth-prime n)))
      1000)))

;; no integers on stack gives 1000 error
(assert (= 1000 (single-error-function "hi" 0)))
;; the right integer on the stack gives 0 error
(assert (= 0 (single-error-function 2 0)))

(defn error-function [program]
  (map (partial single-error-function program) (range 20)))


(def atom-generators
    (concat
      (list
        0
        []

        (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
        (tagged-instruction-erc 1000)
        'in1)
      (registered-for-stacks [:integer :boolean :vector_integer :exec])))


; Define the argmap
(def argmap
  {:error-function error-function
   :atom-generators atom-generators})
   
