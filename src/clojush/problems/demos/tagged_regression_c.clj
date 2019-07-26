;; tagged_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.tagged-regression-c
  (:use [clojush util globals pushstate simplification individual] 
        [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]
        [clojush.instructions.tag]
        [clojush.instructions.environment])
  (:require [clojure.string :as string]
            [local-file]))

;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the 
;; trivial geography chapter) with minimal integer instructions
;; ALSO uses tags, although there is little reason to think they would
;; help on such a simple problem.

;; Before executing the individual programs for the purpose of error calculation, 
;; initialize its tagspace by tagging the whole program with a certain number for tags.
;; Types of values to be tagged include signle instructions/literals and list of instructions/literals
;; enclosed within brackets.


;; http://www.drregex.com/2017/11/match-nested-brackets-with-regex-new.html



;(define-registered
;  myfn
;  ^{:stack-types [:float]}
;  (fn [state]
;    (if (not (empty? (:float state)))
;      (let [item (stack-ref :float 0 state)]
 ;       (->> (pop-item :float state)
;             (push-item (keep-number-reasonable (+ (* item item item) 1)) :float)))
 ;     state)))

;;
;; Implements (x^3+1)^3 + 1 
;;
(def fitness-cases
  (for [input (range -3.5 4.0 0.5)]
    [input
     (let [x-new (+ (* input input input) 1)]
       (+ (* x-new x-new x-new)
          1))]))

(def error-func
  (fn [individual]
      (assoc individual
             :errors
             (doall
              (for [[input target] fitness-cases]
                (let [state (run-push (:program individual)
                                      (push-item input :input
                                                 (push-item input :float (make-push-state))))
                      top-float (top-item :float state)]
                  (if (number? top-float)
                    (abs (- top-float target))      
                    1000.0)))))))


(def argmap
  {:error-function (fn [individual]
                     (let [;stacks-depth (atom (zipmap push-types (repeat 0)))
                           reuse-metric (atom ())       ;the lenght will be equal to the number of test cases
                           repetition-metric (atom ())]
                       (assoc individual
                              :errors (let [ran (rand-nth (range -3.5 4.0 0.5))]
                                        (doall
                                         (for [[input target] fitness-cases]
                                           (let [state (if (= input ran) (run-push (:program (auto-simplify individual
                                                                                                            error-func
                                                                                                            50
                                                                                                            false 100))
                                                                                   (push-item input :input
                                                                                              (push-item input :float (assoc (make-push-state) :calculate-mod-metrics (= input ran)))))
                                                           (run-push (:program individual)
                                                                     (push-item input :input
                                                                                (push-item input :float
                                                                                           (make-push-state)))))
                                                 top-float (top-item :float state)
                                                 ]
                                             (if (= input ran)
                                               (let [metrics (mod-metrics (:trace state) (:trace_id state))]
                                                 (do
                                                   (swap! reuse-metric conj (first metrics))
                                                   (swap! repetition-metric conj (last metrics)))))
                                            ;calculate errors
                                            ; write the regression function here 
                                             (if (number? top-float)
                                               ;(abs (- top-int 
                                               ;        (- (* input input input) 
                                               ;           (* 2 input input) input)))
                                               
                                               (abs (- top-float target))
                                               ;(let [x-new (+ (* input input) 1)]
                                               ;  (abs (- top-int 
                                               ;          (- (* x-new x-new x-new) 
                                               ;             (* 2 x-new x-new) x-new))))
                                               1000.0)))))
                              :reuse-info @reuse-metric 
                              :repetition-info @repetition-metric
                              )
                       ))
   :atom-generators (concat (list (fn [] (lrand 3))
                                  'in1
                                  ;'myfn
                                  )
                            (registered-for-stacks [:float :exec]))
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1}
   :uniform-addition-and-deletion-rate 0.09
   :parent-selection :epsilon-lexicase
   ;:sort-meta-errors-for-lexicase :first
   ;:genetic-operator-probabilities {:alternation 0.5
   ;                                 :uniform-mutation 0.5}
   ;:meta-error-categories [:reuse]
   :filter-params {:features [:reuse] :thresholds [0.75]}
   ;:use-single-thread true
   })