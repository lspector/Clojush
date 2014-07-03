;; odd.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.odd-with-uniform-silence-mutation
  (:use [clojush.pushgp.pushgp]
        [clojush.random]
        [clojush pushstate interpreter]))

;;;;;;;;;;;;
;; The "odd" problem: take a positive integer input and push a Boolean indicating
;; whether or not the input is an odd number. There are many ways to compute this
;; and PushGP sometimes finds unusual methods.

(define-registered 
  in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(def argmap
  {:use-single-thread true
   :error-function (fn [program]
                     (doall
                       (for [input (range 10)]
                         (let [state (run-push program
                                               (push-item input :auxiliary
                                                          (push-item input :integer
                                                                     (make-push-state))))
                               top-bool (top-item :boolean state)]
                           (if (not (= top-bool :no-stack-item))
                             (if (= top-bool (odd? input)) 0 1)
                             1000)))))
   :atom-generators (concat (registered-nonrandom)
                            (list (fn [] (lrand-int 100))
                                  'in))
   :epigenetic-markers [:close :silent]
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.1
                                    [:alternation :uniform-mutation] 0.2 ;Somewhat equivalent to normal Push's ULTRA operator
                                    :uniform-close-mutation 0.1
                                    :uniform-silence-mutation 0.1
                                    }
   :silent-instruction-probability 0.2
   :uniform-silence-mutation-rate 0.1
   })
