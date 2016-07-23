;; factorial.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.integer-regression.factorial
  (:use [clojush.pushgp.pushgp]
        [clojush pushstate interpreter globals]
        [clojure.math.numeric-tower]))

(defn factorial 
  "Returns the factorial of n. Just used to set up fitness cases here, so
   efficiency isn't a concern."
  [n]
  (if (< n 2)
    1
    (* n (factorial (- n 1)))))

(def argmap
  {:error-function (fn [program]
                     (let [behavior (atom '())
                           errors (doall
                                    (for [input (range 1 11)]
                                      (let [state (run-push program
                                                            (push-item input :input
                                                                       (push-item input :integer
                                                                                  (make-push-state))))
                                            top-int (top-item :integer state)]
                                        (when @global-print-behavioral-diversity
                                          (swap! behavior conj top-int))
                                        (if (number? top-int)
                                          (abs (- top-int (factorial input)))
                                          1000000000))))] ;; big penalty, since errors can be big
                       (when @global-print-behavioral-diversity
                         (swap! population-behaviors conj @behavior))
                       errors))
   :atom-generators '(0
                       1
                       in1
                       boolean_and
                       boolean_dup
                       boolean_eq
                       boolean_frominteger
                       boolean_not
                       boolean_or
                       boolean_pop
                       boolean_rot
                       boolean_swap
                       exec_dup
                       exec_eq
                       exec_if
                       exec_k
                       exec_noop
                       exec_pop
                       exec_rot
                       exec_s
                       exec_swap
                       exec_when
                       exec_y
                       integer_add
                       integer_div
                       integer_dup
                       integer_eq
                       integer_fromboolean
                       integer_gt
                       integer_lt
                       integer_mod
                       integer_mult
                       integer_pop
                       integer_rot
                       integer_sub
                       integer_swap
                       )
   :population-size 1000
   :max-generations 500
   :max-points 2000
   :max-genome-size-in-initial-program 100
   :evalpush-limit 1000
   :genetic-operator-probabilities {[:alternation :uniform-mutation] 0.5
                                    [:alternation :uniform-mutation :uniform-close-mutation] 0.5}
   ;   :genetic-operator-probabilities {:uniform-close-mutation 0.1
   ;                                    :alternation 0.45
   ;                                    :uniform-mutation 0.45}
   :alternation-rate 0.05
   :uniform-mutation-rate 0.05
   :alignment-deviation 10
   :parent-selection :lexicase
   :print-history false
   })
