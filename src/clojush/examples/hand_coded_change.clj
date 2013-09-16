(ns clojush.examples.hand-coded-change
  (:use [clojush.examples.change]
        [clojush.evaluate]
        [clojush.individual]
        [clojush.globals]
        [clojush.interpreter]
        [clojush.pushstate]
        [clojush.util]
        [clojush.instructions.tag]
        [clojush.simplification]
        ))


(reset! global-atom-generators change-atom-generators)
(reset! global-evalpush-limit 1000)
(reset! global-max-points-in-program 500)

(defn make-change
  "This is the function that we want evolution to find."
  [amount-in]
  (loop [amount amount-in
         number-coins 0]
    (cond
      (zero? amount) number-coins
      (> 0 amount) "Error: amount is less than 0"
      (<= 25 amount) (recur (- amount 25) (inc number-coins))
      (<= 10 amount) (recur (- amount 10) (inc number-coins))
      (<= 5 amount) (recur (- amount 5) (inc number-coins))
      :else (+ number-coins amount))))


(def hand-coded-change-program
  '(0 integer_swap
      tag_exec_100
      (integer_dup 24 integer_gt exec_if ; check if amount is > 24
                   (25 integer_sub integer_swap 1 integer_add integer_swap tagged_100) ; amount > 24
                   (integer_dup 9 integer_gt exec_if; check if amount is > 9
                                (10 integer_sub integer_swap 1 integer_add integer_swap tagged_100) ; amount > 9
                                (integer_dup 4 integer_gt exec_if; check if amount is > 4
                                             (5 integer_sub integer_swap 1 integer_add integer_swap tagged_100)
                                             (integer_add)))  ; amount > 4
                   ) ; end main loop
      tagged_100
      )
  )

(let [input 19
      ]
  (run-push hand-coded-change-program
            (push-item input :auxiliary
                       (push-item input :integer
                                  (make-push-state)))))

(evaluate-individual (make-individual :program hand-coded-change-program)
                     (change-error-function 150)
                     (new java.util.Random))

;;;;;;;


(def evolved-change-program
  '((((integer_lt 48 integer_mult) exec_k exec_stackdepth integer_fromboolean integer_mod integer_dup integer_dup exec_stackdepth in integer_add integer_add integer_add integer_add 25 integer_div integer_eq exec_rot) exec_pop integer_max in boolean_stackdepth exec_pop integer_stackdepth in boolean_dup 9 exec_yankdup integer_stackdepth -35 exec_stackdepth exec_do*range integer_div integer_yank integer_lt exec_pop exec_y integer_min exec_s integer_rot exec_y integer_min) exec_when integer_rot integer_lt)
  )

#_(48 integer_mult exec_stackdepth integer_fromboolean integer_mod
    integer_dup integer_dup exec_stackdepth in integer_add integer_add
    integer_add integer_add 25 integer_div integer_eq exec_rot exec_pop
    integer_max in boolean_stackdepth exec_pop integer_stackdepth in boolean_dup
    9 exec_yankdup integer_stackdepth -35
    exec_stackdepth exec_do*range integer_div integer_yank integer_lt
    integer_min exec_s integer_rot exec_y integer_min integer_rot integer_lt)


(let [input 19
      ]
  (run-push evolved-change-program
            (push-item input :auxiliary
                       (push-item input :integer
                                  (make-push-state)))))

(evaluate-individual (make-individual :program evolved-change-program)
                     (change-error-function 400)
                     (new java.util.Random))

(auto-simplify-from-program evolved-change-program
                            (change-error-function 150)
                            1000 true 100)
