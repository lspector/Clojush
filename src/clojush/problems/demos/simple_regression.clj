(ns clojush.problems.demos.simple-regression
  (:use [clojush.pushgp.pushgp]
        [clojush.util]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Float symbolic regression of (x^3+1)^5 + 1  with minimal float instructions and an 
;; input instruction that uses the default input stack


;;
;; Implements (x^3+1) as an instruction
;;
;(define-registered
;  myfn
;  ^{:stack-types [:float]}
;  (fn [state]
;    (if (not (empty? (:float state)))
;      (let [item (stack-ref :float 0 state)]
;        (->> (pop-item :float state)
;             (push-item (keep-number-reasonable (+ (* item item item) 1)) :float)))
;      state)))


;;
;; Implements (x^3+1)^5 + 1 
;;
(def fitness-cases
  (for [input (range -3.5 4.0 0.5)]
    [input
     (let [x-new (+ (* input input input) 1)]
       (+ (* x-new x-new x-new)
          1))]))

(def argmap
  {:error-function (fn [individual]
                     (assoc individual
                            :errors
                            (doall
                             (for [[input target] fitness-cases]
                               (let [state (run-push (:program individual)
                                                     (push-item input :input 
                                                                (push-item input :float 
                                                                           (make-push-state))))
                                     top-float (top-item :float state)]
                                 (if (number? top-float)
                                   ;(abs (- top-int 
                                   ;        (- (* input input input) 
                                   ;           (* 2 input input) input)))
                                   (abs (- top-float target))
                                   1000.0))))))
   
   :atom-generators (concat (list (fn [] (lrand 3))
                                  'in1
                                 ; 'myfn
                                  )
                            (registered-for-stacks [:float :exec]))
   :max-generations 500
   :parent-selection :epsilon-lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1}
   :uniform-addition-and-deletion-rate 0.09
   :use-single-thread true
   })
