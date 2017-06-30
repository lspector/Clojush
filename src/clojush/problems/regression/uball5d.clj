;; uball5d.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2017

(ns clojush.problems.regression.uball5d
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Floating point symbolic regression of the Vladislavleva 2009 UBall5D problem.

;; Each training case will be in the form [[x1 x2 x3 x4 x5] y], where the xs are
;; inputs and y is the output.

(def uball5d-training-cases
  (for [xseq (repeatedly 1024
                         (fn [] (repeatedly 5 
                                            #(+ 0.05 (lrand 6.05)))))]
    [xseq
     (->> (map (fn [x] (#(* % %) (- x 3)))
               xseq)
          (reduce +)
          (+ 5)
          (/ 10.0))]))

(def argmap
  {:error-function (fn [individual]
                     (assoc individual
                            :errors
                            (doall
                             (for [[[x1 x2 x3 x4 x5] y] uball5d-training-cases]
                               (let [result (->> (run-push (:program individual)
                                                           (->> (make-push-state)
                                                                (push-item x1 :input)
                                                                (push-item x2 :input)
                                                                (push-item x3 :input)
                                                                (push-item x4 :input)
                                                                (push-item x5 :input)))
                                                 (top-item :float))]
                                 (if (number? result)
                                   (Math/abs (- result y))
                                   1000000))))))
   :atom-generators (conj '[float_div float_mult float_sub float_add
                            float_rot float_swap float_dup float_pop
                            in1 in2 in3 in4 in5]
                          lrand)
   :error-threshold 0.01
   :epigenetic-markers []
   :parent-selection :epsilon-lexicase
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.5}})

