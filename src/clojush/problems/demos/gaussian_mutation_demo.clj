;; gaussian_mutation_demo.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2011

(ns clojush.problems.demos.gaussian-mutation-demo
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Floating point symbolic regression of a polynomial that uses non-integral 
;; constants, but with an ephemeral random constant generator that can
;; only produce integral values. For example, the polynomial includes 1.23
;; while the ephemeral random constant generator can only produce constants
;; like 1.0 and 2.0. While it would be possible to produce 1.23 through
;; arithmetic manipulation of integral values, it is probably easier to 
;; produce it through gaussian mutations, which can happen through uniform
;; mutation.

(def argmap
  {:error-function (fn [individual]
                     (assoc individual
                            :errors
                            (doall
                             (for [input (range -1.0 1.0 0.1)]
                               (let [state (run-push (:program individual)
                                                     (push-item input :input 
                                                                (push-item input :float
                                                                           (make-push-state))))
                                     top-float (top-item :float state)
                                     invalid-output (or (not (number? top-float))
                                                        (= (:termination state) :abnormal))]
                                 (if invalid-output
                                   1000
                                   (abs (- top-float
                                           (+ (* 1.23 input input)
                                              0.73)))))))))
   :atom-generators (concat 
                      '(float_div float_mult float_sub float_add
                                  float_rot float_swap float_dup float_pop)
                      (list 
                        (fn [] (* 1.0 (- (lrand-int 21) 10)))
                        'in1))
   :epigenetic-markers []
   :parent-selection :tournament
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.5}
   :uniform-mutation-constant-tweak-rate 0.8
   :uniform-mutation-float-gaussian-standard-deviation 0.1
   :error-threshold 1.0
   :population-size 5000
   })
