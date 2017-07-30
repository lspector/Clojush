(ns clojush.graphs.generation.report.lexicase
  (:require [plumbing.core :refer [defnk]]
            [plumbing.graph]))

(defnk best-individual [min-error-by-case population]
   (apply max-key
     :n-elite-cases
      population))

(defnk most-zero-cases-best-individual [population]
  (apply max-key
         :n-zero-cases
         population))

(defnk pop-elite-by-case [min-error-by-case population-errors]
  (doall
    (map (fn [errors]
           (map #(if (== %1 %2) 1 0)
                errors
                min-error-by-case))
         population-errors)))

(defnk count-elites-by-case [pop-elite-by-case]
  (doall
    (map #(apply + %) (apply mapv vector pop-elite-by-case))))

(defnk pop-zero-by-case [population-errors]
  (doall
   (map (fn [errors]
          (map #(if (zero? %) 1 0)
               errors))
        population-errors)))

(defnk count-zero-by-case [pop-zero-by-case]
  (doall (map #(apply + %) (apply mapv vector pop-zero-by-case))))

(defnk mean-n-elite-cases [count-elites-by-case population]
  (float (/ (apply + count-elites-by-case) (count population))))

(defnk mean-n-zero-cases [count-zero-by-case population]
   (float (/ (apply + count-zero-by-case) (count population))))

(def graph
  (plumbing.graph/graph
    best-individual
    most-zero-cases-best-individual
    pop-elite-by-case
    count-elites-by-case
    pop-zero-by-case
    count-zero-by-case
    mean-n-elite-cases
    mean-n-zero-cases))
