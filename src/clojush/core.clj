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
  (:require [clojush.graphs.init :refer [->init]]
            [clojush.graphs.utils :refer [end-profile!]]
            [clojush.pushgp.pushgp :refer [pushgp]])
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
  (let [init (->init {:args args})]
    ; call logging functions
    (doseq [_ (vals (:log init))])
    (try
      (do
        (pushgp (:params init))
        (end-profile!))
      (finally
        (shutdown-agents)))))
