;; odd.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.examples.odd-csv-and-json
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]))

;;;;;;;;;;;;
;; The "odd" problem: take a positive integer input and push a Boolean indicating
;; whether or not the input is an odd number. There are many ways to compute this
;; and PushGP sometimes finds unusual methods.

;; This version shows how you can print csv and json log files.

(define-registered 
  in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(define-push-argmap
  :use-single-thread true
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
                           (list (fn [] (rand-int 100))
                                 'in))
  :print-csv-logs true
  :print-json-logs true
  :csv-log-filename "log.csv"
  :json-log-filename "log.json"
  :log-fitnesses-for-all-cases false
  :json-log-program-strings false
  )
