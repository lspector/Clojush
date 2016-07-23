;; clojush.clj
;;
;; This file implements a version of the Push programming language and the PushGP genetic
;; programming system in the Clojure programming language. See the accompanying README
;; file for usage instructions and other notes.
;;
;; Copyright (c) 2010 Lee Spector (lspector@hampshire.edu)
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of version 3 of the GNU General Public License as published by the
;; Free Software Foundation, available from http://www.gnu.org/licenses/gpl.txt.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE. See the GNU General Public License (http://www.gnu.org/licenses/)
;; for more details.

(ns clojush.core
  (:use [clojush.pushgp pushgp report])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main function

(defn -main 
  "A main function for Clojush, which assumes that the first argument is the name
   of a problem file that contains an argmap of arguments to PushGP.
   Exits after completion of the call.
   Any arguments after the first are treated as arguments to PushGP as key-value pairs.
   This allows one to run an example with a call from the OS shell prompt like:
       lein run examples.simple-regression :population-size 3000"
  [& args]
  (println "Command line args:" (apply str (interpose \space args)))
  (let [param-list (map #(if (.endsWith % ".ser")
                           (str %)
                           (read-string %))
                        (rest args))]
    (require (symbol (first args)))
    (let [example-params (eval (symbol (str (first args) "/argmap")))
          params (merge example-params (apply sorted-map param-list))]
      (println "######################################")
      (println "Parameters set at command line or in problem file argmap; may or may not be default:")
      (print-params (into (sorted-map) params))
      (println "######################################")
      (pushgp params)
      (System/exit 0))))
