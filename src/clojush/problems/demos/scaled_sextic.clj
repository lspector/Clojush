(ns clojush.problems.demos.scaled-sextic
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojush.experimental.scaled-errors]
        [clojure.math.numeric-tower]))

;; scaled_sextic.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2011

;;;;;;;;;;;;
;; Floating point symbolic regression of the "sextic polynomial" y=x^6-2x^4+x^2. This uses
;; the core float arithmetic instructions and an input instruction that uses the auxiliary stack.
;; This example also demonstrates the use of Keijzer-style error scaling. See the text following
;; the call to pushgp to see how the scaling information can be extracted to post-process the 
;; outputs of an evolved solution to get the actual outputs.

(defn sextic-scaled-error-function
  ([individual]
    (sextic-scaled-error-function individual false))
  ([individual print-scaling-info]
    (let [inputs (range -1.0 1.0 0.1)
          outputs (doall (map #(top-item :float 
                                         (run-push (:program individual)
                                                   (push-item % :input
                                                              (push-item % :float
                                                                         (make-push-state)))))
                              inputs))
          targets (doall (map #(- (* % % % % % %) (* 2 % % % %) (* % %)) inputs))
          errors (scaled-errors outputs targets 1000000 print-scaling-info)]
      (when print-scaling-info
        (println "outputs" (into [] outputs))
        (println "targets" (into [] targets)))
      (assoc individual :errors errors :behaviors outputs))))

(def argmap
  {:error-function sextic-scaled-error-function
   :error-threshold 0.01
   :atom-generators (concat 
                      '(float_div float_mult float_sub float_add
                                  float_rot float_swap float_dup float_pop)
                      (list 
                        (fn [] (- (lrand 20.0) 10))
                        'in1))
   :population-size 10000
   :epigenetic-markers []
   :parent-selection :tournament
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.5}
   :uniform-mutation-constant-tweak-rate 0.8
   :uniform-mutation-float-gaussian-standard-deviation 0.1
   })

;;;; Here is an evolved solution:
;(def result '(float_dup float_mult float_dup float_add 7.0 float_swap float_dup -2.0 float_sub float_swap -10.0 float_sub float_rot float_swap float_div float_swap float_dup float_dup float_mult float_sub float_mult))

;;;; Here's a call that prints the outputs, targets, slope, and intercept:
;(sextic-scaled-error-function result true)

;;;; This is what the call above prints: 
;outputs [-6.999999999999999 -5.7134939759036145 -4.640851063829788 -3.761639344262297 -3.054925373134329 -2.5 -2.077209302325582 -1.768840864440079 -1.5600000000000003 -1.4394011976047905 -1.4000000000000001 -1.4394011976047905 -1.5600000000000003 -1.7688408644400782 -2.0772093023255813 -2.5 -3.0549253731343278 -3.761639344262294 -4.6408510638297855 -5.713493975903612 -6.999999999999995]
;targets [-2.0 -1.5907590000000003 -1.1970560000000003 -0.8525510000000003 -0.5725440000000002 -0.3593750000000002 -0.20710400000000018 -0.10547100000000012 -0.04313600000000007 -0.010199000000000031 -1.9259299443872359E-32 -0.010198999999999974 -0.043135999999999945 -0.1054709999999999 -0.20710399999999987 -0.3593749999999998 -0.5725439999999996 -0.8525509999999994 -1.1970559999999992 -1.5907589999999991 -1.9999999999999991]
;slope  0.36654287416286585 , intercept  0.5336731760827924

;;;; This shows the actual errors of the evolved solution, when outputs are appropriately unscaled with the calculated slope and intercept.
;;;; The fact that all of these are low shows that the result is reasonably good.
#_(let [outputs [-6.999999999999999 -5.7134939759036145 -4.640851063829788 -3.761639344262297 -3.054925373134329 -2.5 -2.077209302325582 -1.768840864440079 -1.5600000000000003 -1.4394011976047905 -1.4000000000000001 -1.4394011976047905 -1.5600000000000003 -1.7688408644400782 -2.0772093023255813 -2.5 -3.0549253731343278 -3.761639344262294 -4.6408510638297855 -5.713493975903612 -6.999999999999995]
        targets [-2.0 -1.5907590000000003 -1.1970560000000003 -0.8525510000000003 -0.5725440000000002 -0.3593750000000002 -0.20710400000000018 -0.10547100000000012 -0.04313600000000007 -0.010199000000000031 -1.9259299443872359E-32 -0.010198999999999974 -0.043135999999999945 -0.1054709999999999 -0.20710399999999987 -0.3593749999999998 -0.5725439999999996 -0.8525509999999994 -1.1970559999999992 -1.5907589999999991 -1.9999999999999991]
        slope  0.36654287416286585  intercept  0.5336731760827924]
    (map (fn [o t] (- t (+ intercept (* slope o)))) outputs targets))

  
