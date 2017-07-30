;; used in the population attribute of the generation
(ns clojush.graphs.individual
  (:require [plumbing.core :refer [defnk]]
            [plumbing.graph]

            [clojush.graphs.utils :refer [compile-graph]]
            [clojush.util :as util]
            [clojush.simplification :refer [auto-simplify]]))

(defnk program-size [[:individual program]]
  (util/count-points program))

(defnk program-str [[:individual program]]
  (if (and (seq? program)
           (empty? program))
    "()"
    (str program)))

(defnk program-pr-str [[:individual program]]
  (pr-str program))

(defnk program-n-parens [[:individual program]]
  (util/count-parens program))

(defnk program-n-points [[:individual program]]
  (util/count-points program))

(defnk program-percent-parens [program-n-points program-n-parens]
  (double (/ program-n-parens
             program-n-points)))

(defnk genome-size [[:individual genome]]
  (count genome))

(defnk genome-str [[:individual genome]]
  (if (empty? genome)
    "()"
    (str genome)))

(defnk genome-without-uuid-pr-str [[:individual genome]]
  (pr-str (util/not-lazy (map #(dissoc % :uuid :parent-uuid) genome))))

(defnk parent-uuids-str [[:individual parent-uuids]]
  (into [] (map str parent-uuids)))

(defnk error-mean [[:individual total-error errors]]
  (float (/ total-error
            (count errors))))

(defnk partial-simplification-program-pr-str
  [individual
   [:argmap report-simplifications error-function]]
  (pr-str (util/not-lazy (:program (auto-simplify individual
                                                  error-function
                                                  report-simplifications
                                                  false
                                                  1000)))))

(defnk final-simplification
  [individual
   [:argmap error-function final-report-simplifications]]
  (auto-simplify individual
                 error-function
                 final-report-simplifications
                 true
                 500))

(defnk n-elite-cases
  [[:individual errors]
   min-error-by-case]
  (apply + (map #(if (== %1 %2) 1 0)
                errors
                min-error-by-case)))

(defnk n-zero-cases
  [[:individual errors]]
  (apply + (map #(if (zero? %) 1 0)
                errors)))

(def graph
  (plumbing.graph/graph
    program-size
    program-str
    program-pr-str
    program-n-parens
    program-n-points
    program-percent-parens
    genome-size
    genome-str
    genome-without-uuid-pr-str
    parent-uuids-str
    error-mean
    partial-simplification-program-pr-str
    final-simplification
    n-elite-cases
    n-zero-cases))

(def ->individual
  (compile-graph :individual graph))
