;; geometry.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2012

(ns examples.geometry
  (:use [clojush]))

;;;;;;;;;;;;
;; Multiple geometric formulae. An integer indicates the desired formula and
;; any needed inputs are provided on the float stack.

;== Some possibilities for inclusion: 
;perimeter of a square: 4 L
;area of a square: L^2
;perimeter of a rectangle: 2 (L + W)
;area of a rectangle: L W
;circumference of a circle: 2 pi R
;area of a circle: pi R^2
;
;parallelogram...
;rhombus...
;triangle...
;
;volume of a rectangular prism: L W H
;volume of a triangular prism: 
;volume of a cylinder: pi R^2 H
;volume of a cone: 1/3 pi R^2 H
;volume of a sphere: 4/3 pi R^3
;volume of a pyramid: 1/3 B H
;
;Using just circle-related ones for now:
;
;0: circumference of a circle: 2 pi R
;1: area of a circle: pi R^2
;2: volume of a cylinder: pi R^2 H
;3: volume of a cone: 1/3 pi R^2 H
;4: volume of a sphere: 4/3 pi R^3


(def fitness-cases
  (concat
    ;0: circumference of a circle: 2 pi R
    (doall (for [r (range 1 5 0.25)]
             [0 [r 0] (* 2 Math/PI r)]))
    ;1: area of a circle: pi R^2
    (doall (for [r (range 1 5 0.25)]
             [1 [r 0] (* Math/PI r r)]))
    ;2: volume of a cylinder: pi R^2 H
    (doall (for [r (range 1 2 0.25)
                 h (range 1 2 0.25)]
             [2 [r h] (* Math/PI r r h)]))
    ;3: volume of a cone: 1/3 pi R^2 H
    (doall (for [r (range 1 2 0.25)
                 h (range 1 2 0.25)]
             [3 [r h] (* 1/3 Math/PI r r h)]))
    ;4: volume of a sphere: 4/3 pi R^3
    (doall (for [r (range 1 5 0.25)]
             [4 [r 0] (* 4/3 Math/PI r r r)]))))

    
;; input instructions

(define-registered formula
                   (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(define-registered r 
                   (fn [state] (push-item (first (stack-ref :auxiliary 1 state)) :float state)))

(define-registered h
                   (fn [state] (push-item (second (stack-ref :auxiliary 1 state)) :float state)))

;; error function

(def e (fn [program]
         (doall 
           (for [[formula-number inputs target] fitness-cases]
             (let [state (run-push program 
                                   (push-item formula-number 
                                              :auxiliary 
                                              (push-item inputs 
                                                         :auxiliary
                                                         (make-push-state))))
                   top-float (top-item :float state)]
               (if (number? top-float)
                 (Math/abs (- top-float target))
                 1000000))))))
     
;; a partial solution, for testing purposes
#_(reduce + (e '(formula 4 integer_eq exec_if 
                         (r r float_mult r float_mult 3.141592 float_mult 4.0 float_mult 3.0 float_div)
                         (formula 0 integer_eq exec_if
                                  (r 3.141592 float_mult 2.0 float_mult)
                                  (formula 1 integer_eq exec_if
                                           (r r float_mult 3.141592 float_mult)
                                           (r r float_mult h float_mult 3.141592 float_mult))))))


(pushgp 
  :atom-generators (concat (list (fn [] (lrand-int 5))
                                 (fn [] (float (lrand-int 5)))
                                 (fn [] Math/PI)
                                 'formula
                                 'r
                                 'h
                                 (tag-instruction-erc [:exec :integer :float :boolean] 1000)
                                 (tagged-instruction-erc 1000))
                           '(integer_add
                              integer_eq
                              integer_swap
                              ;integer_yank
                              integer_dup
                              ;integer_yankdup
                              integer_lt
                              ;integer_flush
                              ;integer_shove
                              integer_mult
                              ;integer_stackdepth
                              integer_div
                              integer_gt
                              integer_max
                              integer_fromfloat
                              integer_fromboolean
                              integer_sub
                              integer_mod
                              integer_rot
                              integer_min
                              ;integer_rand
                              integer_pop)
                           '(float_lt
                              float_rot
                              ;float_yank
                              float_sin
                              float_frominteger
                              float_cos
                              ;float_stackdepth
                              float_swap
                              float_div
                              ;float_rand
                              ;float_shove
                              float_sub
                              ;float_flush
                              ;float_yankdup
                              float_fromboolean
                              float_gt
                              float_add
                              float_tan
                              float_mult
                              float_max
                              float_pop
                              float_eq
                              float_min
                              float_dup
                              float_mod)
                           '(boolean_swap
                              boolean_eq
                              ;boolean_yank
                              boolean_fromfloat
                              ;boolean_flush
                              boolean_rot
                              boolean_and
                              ;boolean_rand
                              ;boolean_shove
                              boolean_not
                              boolean_or
                              boolean_frominteger
                              ;boolean_stackdepth
                              ;boolean_yankdup
                              boolean_dup
                              boolean_pop)
                           '(;exec_y
                              ;exec_fromziprights
                              exec_pop
                              exec_eq
                              ;exec_stackdepth
                              ;exec_rot
                              ;exec_do*times
                              ;exec_do*count
                              ;exec_s
                              ;exec_do*range
                              ;exec_fromzipnode
                              exec_if
                              ;exec_fromziplefts
                              ;exec_fromzipchildren
                              ;exec_k
                              ;exec_yank
                              ;exec_flush
                              ;exec_yankdup
                              ;exec_fromziproot
                              exec_swap
                              exec_dup
                              ;exec_shove
                              exec_noop))
  :error-function e
  :population-size 1000
  :use-single-thread false
  :error-threshold 0.01
  :max-points 100
  :max-generations 1001
  :mutation-probability 0.4
  :mutation-max-points 20
  :crossover-probability 0.4
  :simplification-probability 0.1
  :tournament-size 7
  :report-simplifications 100
  :final-report-simplifications 1000
  :reproduction-simplifications 10
  :trivial-geography-radius 500
  :decimation-ratio 1
  :decimation-tournament-size 2
  :evalpush-limit 200
  :evalpush-time-limit 0
  :node-selection-method :size-tournament
  :node-selection-leaf-probability 0.1
  :node-selection-tournament-size 2
  :pop-when-tagging true
  :gaussian-mutation-probability 0.0
  :gaussian-mutation-per-number-mutation-probability 0.5
  :gaussian-mutation-standard-deviation 0.1
  :reuse-errors true
  :use-historically-assessed-hardness false 
  )

