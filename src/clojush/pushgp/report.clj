(ns clojush.pushgp.report
  (:use [clojush.util]
        [clojush.globals]
        [clojush.pushstate]
        [clojush.simplification]
        [clojure.data.json :only (json-str)])
  (:require [clojure.string :as string]
            [config :as config]
            [local-file]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

(defn default-problem-specific-report
  "Customize this for your own problem. It will be called at the end of the generational report."
  [best population generation error-function report-simplifications]
  :no-problem-specific-report-function-defined)

(defn git-last-commit-hash
  "Returns the last Git commit hash"
  []
  (let [dir (local-file/project-dir)]
    (string/trim
      (slurp
        (str dir
             "/.git/"
             (subs
               (string/trim
                 (slurp
                   (str dir "/.git/HEAD")))
               5))))))

(defn print-params [push-argmap]
  (doseq [[param val] push-argmap]
    (println (name param) "=" val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log printing (csv and json)

(defn csv-print
  "Prints a csv of the population, with each individual's fitness and size.
   If log-fitnesses-for-all-cases is true, it also prints the value
   of each fitness case."
  [population generation csv-log-filename log-fitnesses-for-all-cases]
  (if (not log-fitnesses-for-all-cases)
    (do
      (when (zero? generation)
        (spit csv-log-filename "generation,individual,total-error,size\n" :append false))
      (doseq [[ind p] (map-indexed vector population)]
        (spit csv-log-filename
              (format "%s,%s,%s,%s\n"
                      generation
                      ind
                      (:total-error p)
                      (count-points (:program p)))
              :append true)))
    (do
      (when (zero? generation)
        (spit csv-log-filename "generation,individual,total-error,size," :append false)
        (spit csv-log-filename
              (format "%s\n"
                      (apply str
                             "TC"
                             (interpose ",TC"
                                        (range (count (:errors (first population)))))))
              :append true))
      (doseq [[ind p] (map-indexed vector population)]
        (spit csv-log-filename
              (format "%s,%s,%s,%s,%s\n"
                      generation
                      ind
                      (:total-error p)
                      (count-points (:program p))
                      (apply str (interpose "," (:errors p))))
              :append true)))))

(defn jsonize-individual
  "Takes an individual and returns it with only the items of interest
   for the json logs."
  [log-fitnesses-for-all-cases json-log-program-strings generation individual]
  (let [part1-ind (-> (if log-fitnesses-for-all-cases
                        {:errors (:errors individual)}
                        {})
                      (assoc :total-error (:total-error individual))
                      (assoc :generation generation)
                      (assoc :size (count-points (:program individual))))
        part2-ind (if json-log-program-strings
                    (assoc part1-ind :program (str (not-lazy (:program individual))))
                    part1-ind)
        part3-ind (if (:hah-error individual)
                    (assoc part2-ind :hah-error (:hah-error individual))
                    part2-ind)]
    (if (:rms-error individual)
      (assoc part3-ind :rms-error (:rms-error individual))
      part3-ind)))

(defn json-print
  "Prints a json file of the population, with each individual's fitness and size.
   If log-fitnesses-for-all-cases is true, it also prints the value
   of each fitness case."
  [population generation json-log-filename log-fitnesses-for-all-cases
   json-log-program-strings]
  (let [pop-json-string (json-str (map #(jsonize-individual
                                          log-fitnesses-for-all-cases
                                          json-log-program-strings
                                          generation
                                          %)
                                       population))]
  (if (zero? generation)
    (spit json-log-filename (str pop-json-string "\n") :append false)
    (spit json-log-filename (str "," pop-json-string "\n") :append true))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; report printing functions

(defn report 
  "Reports on the specified generation of a pushgp run. Returns the best
   individual of the generation."
  ([population generation error-function report-simplifications
    print-csv-logs print-json-logs csv-log-filename json-log-filename
    log-fitnesses-for-all-cases json-log-program-strings print-errors
    print-history]
    (report population generation error-function report-simplifications
            print-csv-logs print-json-logs csv-log-filename json-log-filename
            log-fitnesses-for-all-cases json-log-program-strings
            print-errors print-history
            default-problem-specific-report))
  ([population generation error-function report-simplifications
    print-csv-logs print-json-logs csv-log-filename json-log-filename
    log-fitnesses-for-all-cases json-log-program-strings print-errors
    print-history problem-specific-report]
    (printf "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")(flush)
    ;(println (map :total-error population))(flush) ;***
    (printf "\n;; -*- Report at generation %s" generation)(flush)
    (let [sorted (sort-by :total-error < population)
          best (first sorted)]
      (printf "\nCurrent time: %s" (System/currentTimeMillis))
      (printf "\nBest program: %s" (not-lazy (:program best)))(flush)
      (when (> report-simplifications 0)
        (printf "\nPartial simplification: %s"
                (not-lazy (:program (auto-simplify best error-function report-simplifications false 1000)))))
      (flush)
      (printf "\nCosmos Data: %s" (let [quants (config/quantiles (count population))]
                                    (zipmap quants (map #(:total-error (nth (sort-by :total-error population) %)) quants))))
      (when print-errors (printf "\nErrors: %s" (not-lazy (:errors best))))(flush)
      (printf "\nTotal: %s" (:total-error best))(flush)
      (printf "\nMean: %.4f" (float (/ (:total-error best)
                                       (count (:errors best)))))(flush)
      (printf "\nHAH-error: %s" (:hah-error best))(flush)
      (printf "\nRMS-error: %s" (:rms-error best))(flush)
      (when print-history (printf "\nHistory: %s" (not-lazy (:history best))))(flush)
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
      (when print-csv-logs (csv-print population generation csv-log-filename
                                      log-fitnesses-for-all-cases))
      (when print-json-logs (json-print population generation json-log-filename
                                      log-fitnesses-for-all-cases json-log-program-strings))
      (problem-specific-report best population generation error-function report-simplifications)
      best)))


(defn initial-report
  "Prints the initial report of a PushGP run."
  []
  (printf "\nRegistered instructions: %s\n\n" @registered-instructions) (flush)
  (printf "\nStarting PushGP run.\n\n") (flush)
  (printf "Clojush version = ")
  (try
    (let [version-str (apply str (butlast (re-find #"\".*\""
                                                   (first (string/split-lines
                                                            (local-file/slurp* "project.clj"))))))
          version-number (.substring version-str 1 (count version-str))]
      (if (empty? version-number)
        (throw Exception)
        (printf (str version-number "\n"))))
    (flush)
    (catch Exception e
           (printf "version number unavailable\n")
           (flush)))
  (try
    (let [git-hash (git-last-commit-hash)]
      (if (empty? git-hash)
        (throw Exception)
        (do
          ;; NOTES: - Last commit hash will only be correct if this code has
          ;;          been committed already.
          ;;        - GitHub link will only work if commit has been pushed
          ;;          to GitHub.
          (printf (str "Hash of last Git commit = " git-hash "\n"))
          (printf (str "GitHub link = https://github.com/lspector/Clojush/commit/"
                       git-hash
                       "\n"))
          (flush))))
    (catch Exception e
           (printf "Hash of last Git commit = unavailable\n")
           (printf "GitHub link = unavailable\n")
           (flush))))

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
