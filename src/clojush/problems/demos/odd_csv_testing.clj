;; odd.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.odd-csv-testing
  (:use clojush.pushgp.pushgp)
  (:require clojush.problems.demos.odd))

;;;;;;;;;;;;
;; The "odd" problem: take a positive integer input and push a Boolean indicating
;; whether or not the input is an odd number. There are many ways to compute this
;; and PushGP sometimes finds unusual methods.

;; This version shows how you can print csv and json log files.

(def argmap
  (merge clojush.problems.demos.odd/argmap
         {:print-csv-logs true
          :print-json-logs false
          :csv-log-filename "log.csv"
          :json-log-filename "log.json"
          :csv-columns [:generation :location :parent-uuids :genetic-operators
                        :push-program-size :plush-genome-size
                        :push-program :plush-genome :total-error :test-case-errors]
          :log-fitnesses-for-all-cases true
          :json-log-program-strings false
          }))
