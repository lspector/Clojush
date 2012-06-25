(ns clojush.pushgp.report
  (:use [clojush.util]
        [clojush.globals]
        [clojush.pushgp.simplification]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; report printing functions

(defn default-problem-specific-report
  "Customize this for your own problem. It will be called at the end of the generational report."
  [best population generation error-function report-simplifications]
  :no-problem-specific-report-function-defined)

(defn report 
  "Reports on the specified generation of a pushgp run. Returns the best
   individual of the generation."
  ([population generation error-function report-simplifications]
    (report population generation error-function report-simplifications default-problem-specific-report))
  ([population generation error-function report-simplifications problem-specific-report]
    (printf "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")(flush)
    ;(println (map :total-error population))(flush) ;***
    (printf "\n;; -*- Report at generation %s" generation)(flush)
    (let [sorted (sort-by :total-error < population)
          best (first sorted)]
      (printf "\nCurrent time: %s" (System/currentTimeMillis))
      (printf "\nBest program: %s" (not-lazy (:program best)))(flush)
      (when (> report-simplifications 0)
        (printf "\nPartial simplification (may beat best): %s"
                (not-lazy (:program (auto-simplify best error-function report-simplifications false 1000)))))
      (flush)
      (printf "\nErrors: %s" (not-lazy (:errors best)))(flush)
      (printf "\nTotal: %s" (:total-error best))(flush)
      (printf "\nHAH-error: %s" (:hah-error best))(flush)
      (printf "\nHistory: %s" (not-lazy (:history best)))(flush)
      (printf "\nSize: %s" (count-points (:program best)))(flush)
      (print "\n--- Population Statistics ---\nAverage total errors in population: ")(flush)
      (print (*' 1.0 (/ (reduce +' (map :total-error sorted)) (count population))))(flush)
      (printf "\nMedian total errors in population: %s"
              (:total-error (nth sorted (truncate (/ (count sorted) 2)))))(flush)
      (printf "\nAverage program size in population (points): %s"
              (*' 1.0 (/ (reduce +' (map count-points (map :program sorted)))
                         (count population))))(flush)
      (let [frequency-map (frequencies (map :program population))]
        (println "\nNumber of unique programs in population: " (count frequency-map))
        (println "Max copy number of one program: " (apply max (vals frequency-map)))
        (println "Min copy number of one program: " (apply min (vals frequency-map)))
        (println "Median copy number: " (nth (sort (vals frequency-map)) (Math/floor (/ (count frequency-map) 2)))))
      (printf "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n")
      (flush)
      (problem-specific-report best population generation error-function report-simplifications)
      best)))

(defn final-report
  "Prints the final report of a PushGP run if the run is successful."
  [generation best error-function final-report-simplifications]
  (printf "\n\nSUCCESS at generation %s\nSuccessful program: %s\nErrors: %s\nTotal error: %s\nHistory: %s\nSize: %s\n\n"
          generation (not-lazy (:program best)) (not-lazy (:errors best)) (:total-error best) 
          (not-lazy (:history best)) (count-points (:program best)))
  (when print-ancestors-of-solution
    (printf "\nAncestors of solution:\n")
    (println (:ancestors best)))
  (auto-simplify best error-function final-report-simplifications true 500))
