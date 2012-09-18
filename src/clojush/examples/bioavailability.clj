;; bioavailability.clj
;;
;; Sara Silva and Leonardo Vanneschi. 2009. Operator equalisation,
;; bloat and overfitting: a study on human oral bioavailability
;; prediction. In Proceedings of the 11th Annual conference on
;; Genetic and evolutionary computation (GECCO '09). ACM,
;; New York, NY, USA, 1115-1122. DOI=10.1145/1569901.1570051
;; http://doi.acm.org/10.1145/1569901.1570051 
;;
;; Data available from:
;;  http://personal.disco.unimib.it/Vanneschi/bioavailability.txt
;;
;; Tom Helmuth, thelmuth@cs.umass.edu, 2012

(ns clojush.examples.bioavailability
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [local-file])
  (:require [clojure.string :as string]
            [clojure-csv.core :as csv]))

(defn read-data []
  (let [f (slurp* "src/clojush/examples/data/bioavailability.txt")
        lines (csv/parse-csv f :delimiter \tab)]
    (map #(map (fn [x] (float (read-string x)))
               %)
         lines)))

(count (read-data))

(map last (read-data))
