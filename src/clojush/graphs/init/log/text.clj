(ns clojush.graphs.init.log.text
  (:require [plumbing.core :refer [defnk]]))

(defn print-params [push-argmap]
  (doseq [[param val] push-argmap]
    (println (name param) "=" val)))

(defnk text! [args-str argmap-with-random-str]
  (println "Command line args:" args-str)
  (println "######################################")
  (println "Parameters set at command line or in problem file argmap; may or may not be default:")
  (print-params argmap-with-random-str)
  (println "######################################"))
