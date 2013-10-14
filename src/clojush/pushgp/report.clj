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
  [population generation
   {:keys [error-function report-simplifications print-csv-logs print-json-logs
           csv-log-filename json-log-filename max-generations
           log-fitnesses-for-all-cases json-log-program-strings
           print-errors print-history print-cosmos-data
           problem-specific-report error-threshold use-rmse
           use-historically-assessed-hardness print-timings]}]
  (printf "\n\n") 
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println ";; -*- Report at generation" generation)
  (let [sorted (sort-by :total-error < population)
        best (first sorted)]
    (println "Best program:" (not-lazy (:program best)))
    (when (> report-simplifications 0)
      (println "Partial simplification:"
               (not-lazy (:program (auto-simplify best error-function report-simplifications false 1000)))))
    (when print-errors (println "Errors:" (not-lazy (:errors best))))
    (println "Total:" (:total-error best))
    (println "Mean:" (float (/ (:total-error best)
                               (count (:errors best)))))
    (when use-historically-assessed-hardness
      (println "HAH-error:" (:hah-error best)))
    (when use-rmse (println "RMS-error:" (:rms-error best)))
    (when print-history (println "History:" (not-lazy (:history best))))
    (println "Size:" (count-points (:program best)))
    (println "--- Population Statistics ---")
    (when print-cosmos-data
      (println "Cosmos Data:" (let [quants (config/quantiles (count population))]
                                (zipmap quants (map #(:total-error (nth (sort-by :total-error population) %)) quants)))))
    (println "Average total errors in population:"
             (*' 1.0 (/ (reduce +' (map :total-error sorted)) (count population))))
    (println "Median total errors in population:"
             (:total-error (nth sorted (truncate (/ (count sorted) 2)))))
    (when print-errors (println "Error averages by case:"
                                (apply map (fn [& args] (*' 1.0 (/ (reduce +' args) (count args))))
                                       (map :errors population))))
    (when print-errors (println "Error minima by case:"
                                (apply map (fn [& args] (apply min args))
                                       (map :errors population))))
    (println "Average program size in population (points):"
             (*' 1.0 (/ (reduce +' (map count-points (map :program sorted)))
                        (count population))))
    (let [frequency-map (frequencies (map :program population))]
      (println "Number of unique programs in population:" (count frequency-map))
      (println "Max copy number of one program:" (apply max (vals frequency-map)))
      (println "Min copy number of one program:" (apply min (vals frequency-map)))
      (println "Median copy number:" (nth (sort (vals frequency-map)) (Math/floor (/ (count frequency-map) 2)))))
    (println "--- Timings ---")
    (println "Current time:" (System/currentTimeMillis) "milliseconds")
    (when print-timings
      (let [total-time (apply + (vals @global-timing-map))
            init (get @global-timing-map :initialization)
            reproduction (get @global-timing-map :reproduction)
            fitness (get @global-timing-map :fitness)
            report-time (get @global-timing-map :report)
            other (get @global-timing-map :other)]
        (printf "Total Time:      %8.1f seconds\n" (/ total-time 1000.0))
        (printf "Initialization:  %8.1f seconds, %4.1f%%\n" (/ init 1000.0) (* 100.0 (/ init total-time)))
        (printf "Reproduction:    %8.1f seconds, %4.1f%%\n" (/ reproduction 1000.0) (* 100.0 (/ reproduction total-time)))
        (printf "Fitness Testing: %8.1f seconds, %4.1f%%\n" (/ fitness 1000.0) (* 100.0 (/ fitness total-time)))
        (printf "Report:          %8.1f seconds, %4.1f%%\n" (/ report-time 1000.0) (* 100.0 (/ report-time total-time)))
        (printf "Other:           %8.1f seconds, %4.1f%%\n" (/ other 1000.0) (* 100.0 (/ other total-time)))))
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (flush)
    (when print-csv-logs (csv-print population generation csv-log-filename
                                    log-fitnesses-for-all-cases))
    (when print-json-logs (json-print population generation json-log-filename
                                      log-fitnesses-for-all-cases json-log-program-strings))
    (problem-specific-report best population generation error-function report-simplifications)
    best))


(defn initial-report
  "Prints the initial report of a PushGP run."
  []
  (println "Registered instructions:" @registered-instructions)
  (println "Starting PushGP run.")
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
  [generation best
   {:keys [error-function final-report-simplifications print-ancestors-of-solution]}]
  (printf "\n\nSUCCESS at generation %s\nSuccessful program: %s\nErrors: %s\nTotal error: %s\nHistory: %s\nSize: %s\n\n"
          generation (not-lazy (:program best)) (not-lazy (:errors best)) (:total-error best) 
          (not-lazy (:history best)) (count-points (:program best)))
  (when print-ancestors-of-solution
    (printf "\nAncestors of solution:\n")
    (println (:ancestors best)))
  (auto-simplify best error-function final-report-simplifications true 500))
