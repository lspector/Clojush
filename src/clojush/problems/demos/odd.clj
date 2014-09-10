;; odd.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.odd
  (:use [clojush.pushgp.pushgp]
        [clojush.random]
        [clojush pushstate interpreter]))

;;;;;;;;;;;;
;; The "odd" problem: take a positive integer input and push a Boolean indicating
;; whether or not the input is an odd number. There are many ways to compute this
;; and PushGP sometimes finds unusual methods.

(def argmap
  {:use-single-thread true
   :error-function (fn [program]
                     (doall
                       (for [input (range 10)]
                         (let [state (run-push program
                                               (push-item input :input
                                                          (push-item input :integer
                                                                     (make-push-state))))
                               top-bool (top-item :boolean state)]
                           (if (not (= top-bool :no-stack-item))
                             (if (= top-bool (odd? input)) 0 1)
                             1000)))))
   :atom-generators (concat (registered-nonrandom)
                            (list (fn [] (lrand-int 100))
                                  'in1
                                  'code_rand))
   :meta-error-categories [:size]
   })
