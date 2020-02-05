;; gcd.clj
;; Bill Tozier, bill@vagueinnovation.com
;; updated February 4, 2020
;;
;; This is code for finding algorithms which return the greatest common
;;   divisor (GCD) of a pair of integer arguments. See the Wikipedia
;;   article for background:
;;     https://en.wikipedia.org/wiki/Greatest_common_divisor
;;
;; The two inputs are given as integers using the :input stack, and the result
;; is read from the top of :integer.
;;
;; Some evolved solutions are saved in comments at the end of this file

(ns clojush.problems.software.gcd
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util]
        [clojure.math.numeric-tower]
        ))


; Convenience function to generate test cases
(defn product-of-n-d-sided-dice
  "Produce the product of n randomly-chosen integers from [1,d]"
  [n d]
  (reduce *' 1 (repeatedly n #(inc (rand-int d)))
    ))


; error function
(defn nontrivial-gcd-training-case
  "Given integer scale and multiplicity arguments, produce a triple of two (possibly huge) integers and their actual GCD"
  [scale multiplicity]
  (let [a (product-of-n-d-sided-dice multiplicity scale)
        b (product-of-n-d-sided-dice multiplicity scale)]
        [a b (gcd a b)]
        ))


(defn gcd-coprime-case
  "Given a first integer argument, we scan upwards (by random increments of 1-100) until we find a second argument which is coprime with the first integer; that is, where the pair has GCD=1"
  [arg1]
  (loop [arg2 (+ arg1 (rand-int 100) 1)]
    (if (= 1 (gcd arg1 arg2))
      [arg1 arg2 (gcd arg1 arg2)]
      (recur (+ arg2 (rand-int 100) 1))
      )))


(defn identical-case
  "You can figure this out, I just know it."
  [n]
  [n n n]
  )


(def gcd-training-cases
  "Produces a random sample of 100 training cases: some cases where the integer arguments are identical, some pairs of moderate integers (~10000) with nontrivial GCD values, some pairs of large integers (~10^8) with nontrivial GCD values, and some pairs of coprime integers with GCD=1"
  (into []
    (concat
      (repeatedly 5 #(identical-case (product-of-n-d-sided-dice 20 7)))
      (repeatedly 30 #(nontrivial-gcd-training-case 7 7))
      (repeatedly 50 #(nontrivial-gcd-training-case 20 7))
      (repeatedly 15
        #(gcd-coprime-case (product-of-n-d-sided-dice 7 7)))
        )))


(defn abs-err
  [observed expected]
  (abs (-' observed expected)))


(defn gcd-error-function
  "Given an individual and a collection of training cases, we run the individual's program using the training cases' inputs, and return a vector of absolute errors comparing the observed vs expected GCD values. The behavior (observed results) and error (absolute difference) vectors are associated with the individual as a result."
  [individual cases]
  (let [behaviors (vec (for [a-case cases]
                         (->> (make-push-state)
                              (push-item , (first a-case) :input)
                              (push-item , (nth a-case 1) :input)
                              (run-push , (:program individual))
                              (top-item , :integer))))
        errors (mapv (fn [behavior a-case]
                       (if (int? behavior)
                        (abs-err behavior (last a-case))
                        1000000000000))
                     behaviors
                     cases)]
    (assoc individual
      :behaviors behaviors
      :errors errors)))



; Atom generators
(def gcd-atom-generators
  "Collection of items which will be used to construct random programs: Some integer constants, boolean constants, the input and various standard Push instructions. The relative proportions are stupid and simply the result of poking and prodding with insufficient evidence; feel free to change them."
  (concat
    (repeat 10 'integer_mod)
    (repeat 10 'in1)
    (repeat 10 'in2)
    (list
      10
      1000
      1000000
      1000000000
      2
      3
      5
      7
      11
      19
      31
      53
      true
      false
      ;; constants
      (fn [] (- (lrand-int 21) 10))    ;; Integer ERC [-10,10]
      (fn [] (- (lrand-int 2001) 1000))  ;; Integer ERC [-1000,1000]
      ;;; end input instructions
      )
    (registered-for-stacks [:integer :boolean :exec :code])
      ))



; Define the argmap
(def argmap
  "I literally have no idea what many of these things do. However, with the current settings, the first run I attempted did solve the problem to my satisfaction. YMMV."
  {:error-function (fn [individual]
                      (gcd-error-function
                        individual
                        gcd-training-cases
                        ))
   :atom-generators gcd-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 500
   :evalpush-limit 2000
   :population-size 400
   :max-generations 5000
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 0.5
                                    :alternation 0.5}
   :uniform-addition-and-deletion-rate [0.001 0.01 0.1]
   :alternation-rate [0.001 0.01 0.1]
   :alignment-deviation [0 1 10 100]
   :report-simplifications 0
   :final-report-simplifications 5000
   })

;; An evolved solution (simplified):
;;
;; (in2 in1 integer_mod in1 in1 integer_dup_items integer_mod integer_mod 53 integer_dup_items in1 integer_mod in2 integer_mod integer_mod 2 integer_dup_items integer_mod integer_swap 2 integer_dup_items integer_dec integer_dec integer_mod integer_swap 2 integer_dup_items 2 integer_dup_items integer_mod integer_mod 2 integer_dup_items integer_mod 2 integer_dup_items integer_mod integer_swap 2 integer_dup_items integer_dec integer_dec integer_mod integer_mod 2 integer_dup_items integer_mod 2 integer_dup_items integer_mod integer_swap 2 integer_dup_items integer_dec integer_dec integer_mod integer_mod 2 integer_dup_items integer_mod 2 integer_dup_items integer_mod 2 integer_dup_items integer_mod exec_dup_times ())
