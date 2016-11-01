
(ns clojush.args
  (:require [clj-random.core :as random])
  (:use [clojush globals random util pushstate]
        [clojush.pushgp report]))

(def push-argmap
  (atom (sorted-map
          ;;----------------------------------------
          ;; Clojush system arguments
          ;;----------------------------------------

          :use-single-thread false
          ;; When true, Clojush will only use a single thread.

          :random-seed (random/generate-mersennetwister-seed)
          ;; The seed for the random number generator.

          :run-uuid nil
          ;; This will be set to a new type 4 pseudorandom UUID on every run.

          ;;----------------------------------------
          ;; Standard GP arguments
          ;;----------------------------------------

          :error-function (fn [p] '(0))
          ;; Function that takes a program and returns a list of errors.

          :error-threshold 0
          ;; Pushgp will stop and return the best program if its total error is <= error-threshold.

          :atom-generators (concat @registered-instructions
                                   (list
                                     (fn [] (lrand-int 100))
                                     (fn [] (lrand))))
          ;; The instructions that pushgp will use in random code.

          :population-size 1000
          ;; Number of individuals in the population.

          :max-generations 1001
          ;; The maximum number of generations to run GP.

          :max-point-evaluations 10e100
          ;; The limit for the number of point (instruction) evaluations to execute during the run.

          :max-points 200
          ;; Maximum size of push programs and push code, as counted by points in the program.
          ;; 1/4 this limit is used as the limit for sizes of Plush genomes.

          :max-genome-size-in-initial-program 50
          ;; Maximum size of initial Plush genomes in generation 0. Keep in mind that genome lengths
          ;; will otherwise be limited by 1/4 of :max-points.

          :evalpush-limit 150
          ;; The number of Push instructions that can be evaluated before stopping evaluation.

          :evalpush-time-limit 0
          ;; The time in nanoseconds that a program can evaluate before stopping, 0 means no time
          ;; limit.

          :reuse-errors true
          ;; When true, children produced through direct reproduction will not be re-evaluated but
          ;; will have the error vector of their parent.

          :pass-individual-to-error-function false
          ;; When true, entire individuals (rather than just programs) are passed to error
          ;; functions.

          ;;----------------------------------------
          ;; Genetic operator probabilities
          ;;----------------------------------------

          :genetic-operator-probabilities {:reproduction 0.0
                                           :alternation 0.7
                                           :uniform-mutation 0.1
                                           :uniform-instruction-mutation 0.0
                                           :uniform-integer-mutation 0.0
                                           :uniform-float-mutation 0.0
                                           :uniform-tag-mutation 0.0
                                           :uniform-string-mutation 0.0
                                           :uniform-boolean-mutation 0.0
                                           ; Similar to the old ULTRA operator:
                                           [:alternation :uniform-mutation] 0.2
                                           :uniform-close-mutation 0.0
                                           :uniform-silence-mutation 0.0
                                           :uniform-crossover 0.0
                                           :two-point-crossover 0.0
                                           ; A hill-climbing version of uniform-silence-mutation:
                                           [:make-next-operator-revertable :uniform-silence-mutation] 0.0
                                           :autoconstruction 0.0
                                           :uniform-deletion 0.0
                                           :uniform-addition 0.0
                                           }
          ;; The map supplied to :genetic-operator-probabilities should contain genetic operators
          ;; that sum to 1.0. All available genetic operators are defined in clojush.pushgp.breed.
          ;; Along with single operators, pipelines (vectors) containing multiple operators are
          ;; also allowed, where each operator is applied to the child of the previous operator,
          ;; along with newly selecting individuals where necessary. If an operator is preceeded by
          ;; :make-next-operator-revertable, it will only keep the child if it is at least as good
          ;; as its (first) parent on every test case.

          ;;----------------------------------------
          ;; Arguments related to genetic operators
          ;;----------------------------------------

          :alternation-rate 0.01
          ;; When using alternation, the probability of alternating between the parents when moving
          ;; to the next gene.

          :alignment-deviation 10
          ;; When using alternation, the standard deviation of how far alternation may jump between
          ;; indices when switching between parents.

          :uniform-mutation-rate 0.01
          ;; The probability of each token being mutated during uniform mutation.

          :uniform-mutation-constant-tweak-rate 0.5
          ;; The probability of mutating a constant instead of simply replacing the token
          ;; with a random instruction during uniform mutation.

          :uniform-mutation-float-gaussian-standard-deviation 1.0
          ;; The standard deviation used when tweaking float constants with Gaussian noise.

          :uniform-mutation-int-gaussian-standard-deviation 1
          ;; The standard deviation used when tweaking integer constants with Gaussian noise.

          :uniform-mutation-string-char-change-rate 0.1
          ;; The probability of each character being changed when doing string constant tweaking.

          :uniform-mutation-tag-gaussian-standard-deviation 100
          ;; The standard deviation used when tweaking tag locations with Gaussian noise.

          :uniform-close-mutation-rate 0.1
          ;; The probability of each :close being incremented or decremented during uniform close
          ;; mutation.

          :close-increment-rate 0.2
          ;; The probability of making an increment change to :close during uniform close mutation,
          ;; as opposed to a decrement change.

          :uniform-deletion-rate 0.01
          ;; The probability that any instruction will be deleted during uniform deletion.

          :uniform-addition-rate 0.01
          ;; The probability that any instruction will have a new one added before or after it during
          ;; uniform addition.

          :uniform-silence-mutation-rate 0.1
          ;; The probability of each :silent being switched during uniform silent mutation.

          :replace-child-that-exceeds-size-limit-with :random
          ;; When a child is produced that exceeds the size limit of (max-points / 4), this is
          ;; used to determine what program to return. Options include :parent, :empty, :random,
          ;; :truncate.

          :parent-reversion-probability 1.0
          ;; The probability of a child being reverted to its parent by a genetic operator that
          ;; has been made revertable, if the child is not as good as the parent on at least one
          ;; test case.

          :autoconstructive false
          ;; If true, then :genetic-operator-probabilities will be {:autoconstruction 1.0},
          ;; :epigenetic-markers will be [:close :silent], and :atom-generators will include
          ;; everything in (registered-for-stacks [:integer :boolean :exec :genome :float]). Also sets
          ;; :replace-child-that-exceeds-size-limit-with to :empty. Also, empty-genome individuals
          ;; will not be selected as parents. You will probably also want to provide a high value
          ;; for :max-generations.

          :autoconstructive-diversification-test :gecco2016
          ;; Specifies the diversification test for autoconstruction. The default value of
          ;; :gecco2106, which uses the diversification test used in the work for the
          ;; paper "Evolution Evolves with Autoconstruction" in the ECADA workshop at GECCO 2016.
          ;; For other options and their effects, see the definition of diversifying? in
          ;; genetic-operators.clj.

          :autoconstructive-genome-instructions :all
          ;; Specifies the genome instructions to use for autoconstruction. The default value of
          ;; :all will use all genome instructions. See load-push-argmap in this file (args.clj)
          ;; for other options.

          :autoconstructive-integer-rand-enrichment 0
          ;; The number of extra instances of autoconstructive_integer_rand to include in
          ;; :atom-generators for autoconstruction. If negative then autoconstructive_integer_rand
          ;; will not be in :atom-generators at all.

          :autoconstructive-boolean-rand-enrichment 0
          ;; The number of extra instances of autoconstructive_boolean_rand to include in
          ;; :atom-generators for autoconstruction. If negative then autoconstructive_boolean_rand
          ;; will not be in :atom-generators at all.

          ;;----------------------------------------
          ;; Epignenetics
          ;;----------------------------------------

          :epigenetic-markers [:close]
          ;; A vector of the epigenetic markers that should be used in the individuals.
          ;; Implemented options include: :close, :silent

          :close-parens-probabilities [0.772 0.206 0.021 0.001]
          ;; A vector of the probabilities for the number of parens ending at that position. See
          ;; random-closes in clojush.random

          :silent-instruction-probability 0.2
          ;; If :silent is used as an epigenetic-marker, this is the probability of random
          ;; instructions having :silent be true.

          :track-instruction-maps false
          ;; If true, each Plush instruction map will have a UUID attached to it. If the
          ;; gene has a "parent gene", it will also have the UUID of its parent.
 
          ;;----------------------------------------
          ;; Arguments related to parent selection
          ;;----------------------------------------

          :parent-selection :lexicase
          ;; The parent selection method. Options include :tournament, :lexicase, :epsilon-lexicase,
          ;; :elitegroup-lexicase, :uniform :leaky-lexicase

          :epsilon-lexicase-epsilon nil
          ;; When parent-selection is :epsilon-lexicase,
          ;; the value for epsilon. If nil, automatic epsilon lexicase selection will be used.

          :lexicase-leakage 0.1
          ;; If using leaky lexicase selection, the percentage of selection events that will return
          ;; random (tourny 1) individuals.

          :tournament-size 7
          ;; If using tournament selection, the size of the tournaments.

          :total-error-method :sum
          ;; The method used to compute total error. Options include :sum (standard), :hah
          ;; (historically-assessed hardness), :rmse (root mean squared error), and :ifs
          ;; (implicit fitness sharing).

          :normalization :none
          ;; The method used to normalize the errors to the range [0,1], with 0 being best. Options
          ;; include :none (no normalization), :divide-by-max-error (divides by value of argument
          ;; :max-error), :e-over-e-plus-1 (e/(e+1) = 1 - 1/(e+1))

          :max-error 1000
          ;; If :normalization is set to :max-error, will use this number for normalization.

          :meta-error-categories []
          ;; A vector containing meta-error categories that can be used for parent selection, but
          ;; do not affect total error. See clojush.evaluate for options.

          :trivial-geography-radius 0
          ;; If non-zero, this is used as the radius from which to select individuals for
          ;; tournament or lexicase selection.

          :decimation-ratio 1 ;; If >= 1, does nothing. Otherwise, is the percent of the population
          ;; size that is retained before breeding. If 0 < decimation-ratio < 1, decimation
          ;; tournaments will be used to reduce the population to size (* population-size
          ;; decimation-ratio) before breeding.

          :decimation-tournament-size 2
          ;; Size of the decimation tournaments.

          :print-selection-counts false
          ;; If true, keeps track of and prints the number of times each individual was selected
          ;; to be a parent

          ;;----------------------------------------
          ;; Arguments related to the Push interpreter
          ;;----------------------------------------

          :pop-when-tagging true
          ;; When true, tagging instructions will pop the exec stack when tagging; otherwise, the
          ;; exec stack is not popped.

          :tag-limit 10000
          ;; The size of the tag space.

          :top-level-push-code false
          ;; When true, run-push will push the program's code onto the code stack prior to running.

          :top-level-pop-code false
          ;; When true, run-push will pop the code stack after running the program.

          ;;----------------------------------------
          ;; Arguments related to generational and final reports
          ;;----------------------------------------

          :report-simplifications 100
          ;; The number of simplification steps that will happen during report simplifications

          :final-report-simplifications 1000
          ;; The number of simplification steps that will happen during final report
          ;; simplifications.

          :problem-specific-initial-report default-problem-specific-initial-report
          ;; A function can be called to provide a problem-specific initial report, which happens
          ;; before the normal initial report is printed.

          :problem-specific-report default-problem-specific-report
          ;; A function can be called to provide a problem-specific report, which happens before
          ;; the normal generational report is printed.

          :return-simplified-on-failure false
          ;; When true, will simplify the best indivual and return it, even if the error threshold
          ;; has not been reached. This will make failures return the same as successes.

          :print-errors true
          ;; When true, prints the error vector of the best individual.

          :print-history false
          ;; When true, prints the history of the best individual's ancestors' total errors.

          :print-timings false
          ;; If true, report prints how long different parts of evolution have taken during the
          ;; current run.

          :print-error-frequencies-by-case false
          ; If true, print reports of error frequencies by case each generation.

          :print-cosmos-data false
          ;; If true, report prints COSMOS data each generation.

          :maintain-ancestors false
          ;; If true, save all ancestors in each individual (costly).

          :print-ancestors-of-solution false
          ;; If true, final report prints the ancestors of the solution. Requires
          ;; :maintain-ancestors to be true.

          :print-behavioral-diversity false
          ;; If true, prints the behavioral diversity of the population each generation.
          ;; Note: The error function for the problem must support behavioral diversity.
          ;; For an example, see wc.clj

          :print-homology-data false
          ;; If true, prints the homology statistics.

          ;;----------------------------------------
          ;; Arguments related to printing JSON, EDN, or CSV logs
          ;;----------------------------------------

          :print-csv-logs false
          ;; Prints a CSV log of the population each generation.

          :print-edn-logs false
          ;; Prints an EDN log of the run.

          :print-json-logs false
          ;; Prints a JSON log of the population each generation.

          :csv-log-filename "log.csv"
          ;; The file to print CSV log to.

          :edn-log-filename "log.edn"
          ;; The file to print EDN log to.

          :json-log-filename "log.json"
          ;; The file to print JSON log to.

          :csv-columns [:generation :location :total-error :push-program-size]
          ;; The columns to include in a printed CSV beyond the generation and individual. Options
          ;; include: [:generation :location :parent-uuids :genetic-operators :push-program-size
          ;; :plush-genome-size :push-program :plush-genome :total-error :test-case-errors]

          :edn-keys [:uuid :parent-uuids :genetic-operators :program :genome :total-error :errors]
          ;; Keys from clojush.individual.individual that should be included.

          :edn-additional-keys [:generation :location]
          ;; Additional information to include in the edn-printout. Available options are
          ;; [:generation :location :push-program-size :plush-genome-size].

          :log-fitnesses-for-all-cases false
          ;; If true, the CSV and JSON logs will include the fitnesses of each individual on every
          ;; test case.

          :json-log-program-strings false
          ;; If true, JSON logs will include program strings for each individual.
          )))

(defn load-push-argmap
  [argmap]
  (doseq [[argkey argval] argmap]
    (assert (contains? @push-argmap argkey) (str "Argument key " argkey " is not a recognized argument to pushgp."))
    (swap! push-argmap assoc argkey argval))
  (swap! push-argmap assoc :run-uuid (java.util.UUID/randomUUID))
  ;; Augmentation for autoconstruction
  (when (:autoconstructive @push-argmap)
    (swap! push-argmap assoc :genetic-operator-probabilities {:autoconstruction 1.0})
    (swap! push-argmap assoc :epigenetic-markers [:close :silent])
    (doseq [instr (case (:autoconstructive-genome-instructions @push-argmap)
                    :all (registered-for-stacks [:integer :boolean :exec :genome :float])
                    :gene-oriented (concat (registered-for-stacks [:integer :boolean :exec :float])
                                           '(genome_pop
                                              genome_dup
                                              genome_swap
                                              genome_rot
                                              genome_flush
                                              genome_eq
                                              genome_stackdepth
                                              genome_yank
                                              genome_yankdup
                                              genome_shove
                                              genome_empty
                                              genome_gene_dup
                                              genome_gene_randomize
                                              genome_gene_replace
                                              genome_gene_delete
                                              genome_rotate
                                              genome_gene_copy
                                              genome_gene_copy_range
                                              genome_toggle_silent
                                              genome_silence
                                              genome_unsilence
                                              genome_close_inc
                                              genome_close_dec
                                              genome_new
                                              genome_parent1
                                              genome_parent2
                                              autoconstructive_integer_rand
                                              autoconstructive_boolean_rand))
                    :uniform (concat (registered-for-stacks [:integer :boolean :exec :float])
                                     '(genome_pop
                                        genome_dup
                                        genome_swap
                                        genome_rot
                                        genome_flush
                                        genome_eq
                                        genome_stackdepth
                                        genome_yank
                                        genome_yankdup
                                        genome_shove
                                        genome_empty
                                        genome_rotate
                                        genome_new
                                        genome_parent1
                                        genome_parent2
                                        autoconstructive_integer_rand
                                        autoconstructive_boolean_rand
                                        genome_uniform_instruction_mutation
                                        genome_uniform_integer_mutation
                                        genome_uniform_float_mutation
                                        genome_uniform_tag_mutation
                                        genome_uniform_string_mutation
                                        genome_uniform_boolean_mutation
                                        genome_uniform_close_mutation
                                        genome_uniform_silence_mutation
                                        genome_uniform_deletion
                                        genome_uniform_addition
                                        genome_alternation
                                        genome_uniform_crossover)))]
      (when (not (some #{instr} (:atom-generators @push-argmap)))
        (swap! push-argmap assoc :atom-generators (conj (:atom-generators @push-argmap) instr))))
    (swap! push-argmap assoc :atom-generators (conj (:atom-generators @push-argmap) (fn [] (lrand))))
    (swap! push-argmap assoc :atom-generators (conj (:atom-generators @push-argmap) (fn [] (lrand-int 100))))
    (dotimes [n (:autoconstructive-integer-rand-enrichment @push-argmap)]
      (swap! push-argmap assoc :atom-generators (conj (:atom-generators @push-argmap) 'autoconstructive_integer_rand)))
    (if (neg? (:autoconstructive-integer-rand-enrichment @push-argmap))
      (swap! push-argmap assoc :atom-generators (remove #(= % 'autoconstructive_integer_rand)
                                                        (:atom-generators @push-argmap))))
    (dotimes [n (:autoconstructive-boolean-rand-enrichment @push-argmap)]
      (swap! push-argmap assoc :atom-generators (conj (:atom-generators @push-argmap) 'autoconstructive_boolean_rand)))
    (if (neg? (:autoconstructive-boolean-rand-enrichment @push-argmap))
      (swap! push-argmap assoc :atom-generators (remove #(= % 'autoconstructive_boolean_rand)
                                                        (:atom-generators @push-argmap))))
    (swap! push-argmap assoc :replace-child-that-exceeds-size-limit-with :empty)))

(defn reset-globals
  "Resets all Clojush globals according to values in @push-argmap. If an argmap argument is provided then it is loaded
  into @push-argmap first."
  ([]
   (doseq [[gname gatom] (filter (fn [[a _]] (.startsWith (name a) "global-")) (ns-publics 'clojush.globals))]
     (if (contains? @push-argmap (keyword (.substring (name gname) (count "global-"))))
       (reset! @gatom (get @push-argmap (keyword (.substring (str gname) (count "global-")))))
       (throw (Exception. (str "globals.clj definition " gname " has no matching argument in push-argmap. Only such definitions should use the prefix 'global-'."))))))
  ([argmap]
   (load-push-argmap argmap)
   (reset-globals)))

