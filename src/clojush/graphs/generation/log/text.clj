(ns clojush.graphs.generation.log.text
  (:require [plumbing.core :refer [defnk]]
            [clj-random.core :as random]
            [clojush.util :refer [not-lazy]]))

(defnk lexicase-report
  "This extra report is printed whenever lexicase selection is used."
  [lexicase
   [:argmap report-simplifications
             print-errors
             meta-error-categories
             print-history]]

  (doseq [[individual short-name header-name] [[(:best-individual lexicase)
                                                "Lexicase"
                                                "Elite"]
                                               [(:most-zero-cases-best-individual lexicase)
                                                "Zero cases"
                                                "Zero"]]]
    (println "--- Lexicase Program with Most" header-name "Cases Statistics ---")
    (println short-name "best genome:" (:genome-without-uuid-pr-str individual))
    (println short-name "best program:" (:program-pr-str individual))
    (when (> report-simplifications 0)
      (println short-name "best partial simplification:"
               (:partial-simplification-program-pr-str individual)))
    (when print-errors (println short-name "best errors:" (not-lazy (:errors individual))))
    (when (and print-errors (not (empty? meta-error-categories)))
      (println short-name "best meta-errors:" (not-lazy (:meta-errors individual))))
    (println short-name "best number of elite cases:" (:n-elite-cases individual))
    (when (= short-name "Zero cases")
      (println short-name "best number of zero cases:" (:n-zero-cases individual)))
    (println short-name "best total error:" (:total-error individual))
    (println short-name "best mean error:" (:error-mean individual))
    (when print-history (println short-name "best history:" (not-lazy (:history individual))))
    (println short-name "best size:" (:program-size individual))
    (printf "Percent parens: %.3f\n"
            (:program-percent-parens individual))) ;Number of (open) parens / points

  (println "--- Lexicase Population Statistics ---")
  (println "Count of elite individuals by case:" (:count-elites-by-case lexicase))
  (println (format "Population mean number of elite cases: %.2f"
                  (:mean-n-elite-cases lexicase)))
  (println "Count of perfect (error zero) individuals by case:"
    (:count-zero-by-case lexicase))
  (println (format "Population mean number of perfect (error zero) cases: %.2f"
                   (:mean-n-zero-cases lexicase))))

(defnk implicit-fitness-sharing-report
  [ifs-best
   [:argmap print-errors meta-error-categories]]

  (println "--- Program with Best Implicit Fitness Sharing Error Statistics ---")
  (println "IFS best genome:" (:genome-without-uuid-pr-str ifs-best))
  (println "IFS best program:" (:program-pr-str ifs-best))
  (when print-errors (println "IFS best errors:" (not-lazy (:errors ifs-best))))
  (when (and print-errors (not (empty? meta-error-categories)))
    (println "IFS best meta-errors:" (not-lazy (:meta-errors ifs-best))))
  (println "IFS best total error:" (:total-error ifs-best))
  (println "IFS best mean error:" (:error-mean ifs-best))
  (println "IFS best IFS error:" (:weighted-error ifs-best))
  (println "IFS best size:" (:program-n-points ifs-best))
  (printf "IFS best percent parens: %.3f\n"
    (:program-percent-parens ifs-best)))

(defnk text
  [[:config argmap]
   report
   index
   population]
  ;; evaluate first so its' counted
  (:point-evaluations-before-report report)
  (println)
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println ";; -*- Report at generation" index)
  ;; acccess the best so that the problem specific report is printed
  (:best report)
  (when (:print-error-frequencies-by-case argmap)
    (println "Error frequencies by case:"
      (:error-frequencies-by-case report)))
  (when (some #{(:parent-selection argmap)}
              #{:lexicase :elitegroup-lexicase :leaky-lexicase :epsilon-lexicase
                :random-threshold-lexicase})
        (lexicase-report {:argmap argmap :lexicase (:lexicase report)}))
  (when (= (:total-error-method argmap) :ifs)
    (implicit-fitness-sharing-report {:ifs-best (:ifs-best report) :argmap argmap}))
  (println (format "--- Best Program (%s) Statistics ---" (str "based on " (name (:err-fn report)))))
  (let [best (:best report)]
    (println "Best genome:" (:genome-without-uuid-pr-str best))
    (println "Best program:" (:program-pr-str best))
    (when (> (:report-simplifications argmap) 0)
      (println "Partial simplification:"
               (:partial-simplification-program-pr-str best)))
    (when (:print-errors argmap) (println "Errors:" (not-lazy (:errors best))))
    (when (and (:print-errors argmap) (not (empty? (:meta-error-categories argmap))))
      (println "Meta-Errors:" (not-lazy (:meta-errors best))))
    (println "Total:" (:total-error best))
    (println "Mean:" (:error-mean best))
    (when (not= (:normalization argmap) :none)
      (println "Normalized error:" (:normalized-error best)))
    (case (:total-error-method argmap)
      :hah (println "HAH-error:" (:weighted-error best))
      :rmse (println "RMS-error:" (:weighted-error best))
      :ifs (println "IFS-error:" (:weighted-error best))
      nil)
    (when (:print-history argmap) (println "History:" (not-lazy (:history best))))
    (when (= (:parent-selection argmap) :novelty-search)
      (println "Novelty: " (float (:novelty best))))
    (println "Genome size:" (:genome-size best))
    (println "Size:" (:program-size best))
    (printf "Percent parens: %.3f\n"
      (:program-percent-parens best))) ;Number of (open) parens / points
  (println "--- Population Statistics ---")
  (when (:print-cosmos-data argmap)
    (println "Cosmos Data:" (:cosmos-data report)))
  (println "Average total errors in population:"
        (-> report :total-error-stats :mean float))
  (println "Median total errors in population:"
        (-> report :total-error-stats :median))
  (when (:print-errors argmap)
    (println "Error averages by case:"
                              (:error-by-case-mean report)))
  (when (:print-errors argmap)
    (println "Error minima by case:"
                              (:error-by-case-min report)))
  (when (and (:print-errors argmap) (not (empty? (:meta-error-categories argmap))))
    (println "Meta-Error averages by category:"
             (:meta-error-by-category-mean report))
    (println "Meta-Error minima by category:"
             (:meta-error-by-category-min report)))
  (println "Average genome size in population (length):"
    (->> report :genome-size-stats :mean float))
  (println "Average program size in population (points):"
    (->> report :program-size-stats :mean float))
  (printf "Average percent parens in population: %.3f\n"
    (->> report :program-percent-parens-stats :mean))
  (let [stats (:age-stats report)]
    (println "Minimum age in population:" (float (:min stats)))
    (println "Maximum age in population:" (float (:max stats)))
    (println "Average age in population:" (float (:mean stats)))
    (println "Median age in population:" (float (:median stats))))
  (let [stats (:grain-size-stats report)]
    (println "Minimum grain-size in population:" (float (:min stats)))
    (println "Maximum grain-size in population:" (float (:max stats)))
    (println "Average grain-size in population:" (float (:mean stats)))
    (println "Median grain-size in population:" (float (:median stats))))
  (println "--- Population Diversity Statistics ---")
  (let [diversity (:genome-diversity report)]
    (println "Min copy number of one Plush genome:"
      (-> diversity :frequency-stats :min))
    (println "Median copy number of one Plush genome:"
      (-> diversity :frequency-stats :median))
    (println "Max copy number of one Plush genome:"
      (-> diversity :frequency-stats :max))
    (println "Genome diversity (% unique Plush genomes):\t"
      (:percent-unique diversity)))
  (let [diversity (:program-diversity report)]
    (println "Min copy number of one Push program:"
      (-> diversity :frequency-stats :min))
    (println "Median copy number of one Push program:"
      (-> diversity :frequency-stats :median))
    (println "Max copy number of one Push program:"
      (-> diversity :frequency-stats :max))
    (println "Syntactic diversity (% unique Push programs):\t"
      (:percent-unique diversity)))
  (println "Total error diversity:\t\t\t\t"
      (get-in report [:total-error-diversity :percent-unique]))
  (println "Error (vector) diversity:\t\t\t"
      (get-in report [:errors-diversity :percent-unique]))
  (when (not (nil? (:behaviors (first population))))
    (println "Behavioral diversity:\t\t\t\t"
      (get-in report [:behaviors-diversity :percent-unique])))
  (when (:print-homology-data argmap)
    (let [stats (:homology-stats report)]
      (println "--- Population Homology Statistics (all stats reference the sampled population edit distance of programs) ---")
      (println "Number of homology samples:" (:n stats))
      (println "Average:            " (:mean stats))
      (println "Standard deviation: " (:standard-deviation stats))
      (println "First quartile: " (:first-quartile stats))
      (println "Median:         " (:median stats))
      (println "Third quartile: " (:third-quartile stats))))

  (when (:print-selection-counts argmap)
    (println "Selection counts:"
             (:selection-counts-sorted report))
    ((:reset-selection-counts! report)))
  (when (:autoconstructive argmap)
    (println "Number of random replacements for non-diversifying individuals:"
      (:non-diversifying-n report)))
  (println "--- Run Statistics ---")
  (println "Number of program evaluations used so far:"
    @(:evaluations-count report))
  (println "Number of point (instruction) evaluations so far:"
    (:point-evaluations-before-report report))
  ((:reset-point-evaluations-count! report))
  (println "--- Timings ---")
  (println "Current time:" (System/currentTimeMillis) "milliseconds")
  (when (:print-timings argmap)
    (let [seconds (:timing-map-seconds report)
          percent (:timing-map-percent report)]
      (printf "Total Time:      %8.1f seconds\n"
              (:timing-map-total-seconds report))
      (printf "Initialization:  %8.1f seconds, %4.1f%%\n"
              (:initialization seconds) (:initialization percent))
      (printf "Reproduction:    %8.1f seconds, %4.1f%%\n"
              (:reproduction seconds) (:reproduction percent))
      (printf "Fitness Testing: %8.1f seconds, %4.1f%%\n"
              (:fitness seconds) (:fitness percent))
      (printf "Report:          %8.1f seconds, %4.1f%%\n"
              (:report seconds) (:report percent))
      (printf "Other:           %8.1f seconds, %4.1f%%\n"
              (:other seconds) (:other percent))))
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println ";; -*- End of report for generation" index)
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (flush)
  (when (= :success (:outcome report))
    (let [best (:best report)]
      (printf "\n\nSUCCESS at generation %s\nSuccessful program: %s\nErrors: %s\nTotal error: %s\nHistory: %s\nSize: %s\n\n"
              index (:program-str best) (:errors best) (:total-error best)
              (:history best) (:program-n-points best))
      (when (:print-ancestors-of-solution argmap)
        (printf "\nAncestors of solution:\n")
        (prn (:ancestors best)))
      ;; call first so logs come first
      (:final-simplification best)
      (println "\n;;******************************")
      (println ";; Problem-Specific Report of Simplified Solution")
      (:problem-specific-report-final-simplified-best report)))
  (when (= :failure (:outcome report))
    (printf "\nFAILURE\n")))
