;; tagged_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.tagged-regression5-cc
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
;; Implements (x^5+1)^5 + 1
;;
(def fitness-cases
  (for [input (range -3.5 4.0 0.5)]
    [input
     (let [x-new (+ (* input input input input input) 1)]
       (+ (* x-new x-new x-new x-new x-new)
          1))]))

(def exec-reuse-instrs '(exec_dup, exec_dup_times, exec_dup_items, exec_yankdup, exec_do*range, exec_do*count,  exec_do*times, exec_while, exec_do*while, exec_s, exec_y, exec_do*vector_integer, exec_do*vector_float, exec_do*vector_boolean, exec_do*vector_string))

(def argmap
  {:error-function (fn error-function
                     ([individual]
                      (error-function individual :train))
                     ([individual data-cases] ;; data-cases should be :train or :test
                      (error-function individual data-cases false))
                     ([individual data-cases print-outputs]
                      (let [errors (doall
                                     (for [[input target] fitness-cases]
                                       (let [state (run-push (:program individual)
                                                             (push-item input :input
                                                                        (push-item input :float
                                                                                   (make-push-state))))
                                             top-float (top-item :float state)]
                                         ;calculate errors
                                         (if (number? top-float)
                                           (abs (- top-float target))
                                           1000.0))))]
                        (assoc individual
                          :errors errors)
                        )))
   :atom-generators (concat (list (fn [] (lrand 3))
                                  'in1          
                                  ;(tag-instruction-erc [:float :exec] 100)
                                  ;(untag-instruction-erc 100)
                                  ;(tagged-instruction-erc 100)
                                  )
                            ;(remove (set exec-reuse-instrs) (registered-for-stacks [:float :exec]))
                            (registered-for-stacks [:float :integer :exec])
                            )
   :genetic-operator-probabilities {:uniform-addition-and-deletion 0.5
                                    [:uniform-segment-duplication :uniform-segment-deletion] 0.5}
   :uniform-segmenting-rate 0.09
   :uniform-segment-duplication-rate 0.09
   :uniform-segment-deletion-rate 0.09
   :uniform-addition-and-deletion-rate 0.09
   :parent-selection :epsilon-lexicase
   :max-generations 500
   :report-simplifications 0
   ;:print-history true
   })
