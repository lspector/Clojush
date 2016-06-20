
;============== NOTE NOTE NOTE =================
; This file has note been updated for Clojush 2.0, and will likely not work
;============== NOTE NOTE NOTE =================

(ns clojush.problems.boolean.valiant
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

(def numvars 100) ;1000)
(def numinputs 50) ;500)
(def numcases 500) ;5000)

(def input-indices 
  (vec (take numinputs (shuffle (range numvars)))))

(def cases 
  (vec (repeatedly numcases
                   (fn [] (let [vars (vec (repeatedly numvars #(< (lrand) 0.5)))]
                            [vars (even? (count (filter #(= % true) 
                                                        (map #(nth vars %)
                                                             input-indices))))])))))

(println "input-indices:" input-indices)

;; ugly way to define all of the input instructions, since I haven't fully
;; grokked Clojure macros

(def n-hack (atom 0))

(defn define-input [n]
  (reset! n-hack n)
  (eval `(define-registered ~(symbol (str "input" @n-hack))
                            (fn [state#] 
                              (push-item (nth (first (:auxiliary state#)) ~(+ @n-hack))
                                         :boolean
                                         state#)))))

(dotimes [n numvars] (define-input n))
 
;; this gives the instructions: (registered-for-type "input")

(defn rp [prog state] (run-push prog state true))

(defn valiant-fitness [program]
  (doall (for [c (range numcases)]
           (let [[inputs answer] (nth cases c)
                 output (->> (make-push-state)
                             (push-item inputs :auxiliary)
                             (run-push program)
                             ;(rp program)
                             (top-item :boolean))]
             ;(println "output" output "answer" answer)
             (if (= output answer) 0 1)))))

;input-indices

;(reduce + (valiant-fitness '(input1 input4 input0 input7 input9 boolean_and boolean_and boolean_and boolean_and)))

;; oversized-offspring-fail-to-random? -- take fail to random code from here and use right
;; parameters
;(in-ns 'clojush.pushgp.genetic-operators)
;(defn crossover 
;  "Returns a copy of parent1 with a random subprogram replaced with a random 
;   subprogram of parent2."
;  [parent1 parent2 max-points]
;  (let [new-program (case (lrand-int 2)
;                      0 (insert-code-at-point 
;                          (:program parent1) 
;                          (select-node-index (:program parent1))
;                          (code-at-point (:program parent2)
;                                         (select-node-index (:program parent2))))
;                      1 (list (random-code 10 @global-atom-generators) 'exec_if (:program parent1) (:program parent2)))]
;    (if (> (count-points new-program) max-points)
;      ;parent1
;      (make-individual :program (random-code 10 @global-atom-generators) :history (:history parent1)
;                       :ancestors (if global-maintain-ancestors
;                                    (cons (:program parent1) (:ancestors parent1))
;                                    (:ancestors parent1)))
;      (make-individual :program new-program :history (:history parent1)
;                       :ancestors (if global-maintain-ancestors
;                                    (cons (:program parent1) (:ancestors parent1))
;                                    (:ancestors parent1))))))
;(in-ns 'experimental.valiant)
;; 
;; Probabilistic Pseudo Hillclimbing (persistence)
;;

;(def argmap
;  {:error-function valiant-fitness
;   :atom-generators (concat (vec (registered-for-type "input"))
;                            ;)
;                            (apply concat 
;                                   (repeat 25 
;                                           '(boolean_and boolean_or boolean_not exec_if))))
;   :use-lexicase-selection true
;   :max-points 40000
;   :max-genome-size-in-initial-program 10
;   :population-size 100
;   :evalpush-limit 10000
;   :mutation-probability 0.4
;   :mutation-max-points 50
;   ;:crossover-probability 0.4
;   :simplification-probability 0.2
;   :reproduction-simplifications 1
;   ;:deletion-mutation-probability 0.2
;   :boolean-gsxover-probability 0.4
;   :boolean-gsxover-new-code-max-points 10
;   :parent-reversion-probability 0.95
;   ;:decimation-ratio 0.01
;   ;:use-single-thread true
;   })

(def argmap
  {:error-function valiant-fitness
   :atom-generators (concat (vec (registered-for-type "input"))
                            ;)
                            (apply concat 
                                   (repeat 25 
                                           '(boolean_and boolean_or boolean_not exec_if))))
   :max-points 4000
   :max-genome-size-in-initial-program 100
   :population-size 100
   :evalpush-limit 2000
   :genetic-operator-probabilities {[:alternation :uniform-mutation] 1.0} ;Somewhat equivalent to normal Push's ULTRA operator
   :alignment-deviation 20
   ;:use-single-thread true
   })
