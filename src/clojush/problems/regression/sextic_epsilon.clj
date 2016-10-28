;; sextic.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.regression.sextic-epsilon
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Floating point symbolic regression of the "sextic polynomial" y=x^6-2x^4+x^2. This uses
;; the core float arithmetic instructions and an input instruction that uses the input stack.
;; This example also demonstrates  the use of fitness penalties for abnormally terminating programs.
;; It also uses squared errors in the error function and a non-zero error threshold.

(def argmap
  {:error-function (fn [program]
                     (doall
                       (for [input (range -1.0 1.0 0.1)]
                         (let [state (run-push program 
                                               (push-item input :input 
                                                          (push-item input :float
                                                                     (make-push-state))))
                               top-float (top-item :float state)
                               invalid-output (or (not (number? top-float))
                                                  (= (:termination state) :abnormal))]
                           (if invalid-output
                             1000
                             (expt (- top-float
                                      (+ (* input input input input input input)
                                         (- (* 2 input input input input))
                                         (* input input)))
                                   2))))))
   :error-threshold 0.01
   :atom-generators (concat 
                      '(float_div float_mult float_sub float_add
                                  float_rot float_swap float_dup float_pop)
                      (list 
                        (fn [] (* 1.0 (- (lrand-int 21) 10)))
                        'in1))
   :population-size 1000
   :epigenetic-markers []
   :parent-selection :epsilon-lexicase
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.5}
   :uniform-mutation-rate 0.1
   :alternation-rate 0.1
   :alignment-deviation 100
   :uniform-mutation-constant-tweak-rate 0.8
   :uniform-mutation-float-gaussian-standard-deviation 0.1
   })
