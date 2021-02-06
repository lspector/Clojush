(ns clojush.args
  (:require [clj-random.core :as random])
  (:use [clojush globals random util pushstate]
        [clojush.instructions.tag]
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
          ;; Pushgp will stop and return the best program if its total error
          ;; is <= error-threshold.

         :atom-generators (into @registered-instructions
                                (list
                                 (fn [] (lrand-int 100))
                                 (fn [] (lrand))))
          ;; The instructions that pushgp will use in random code.
         
         :population-size 1000
          ;; Number of individuals in the population.

         :max-generations 1001
          ;; The maximum number of generations to run GP.

         :max-program-executions 10e100
          ;; The maximum number of program executions (running on a single case
          ;; counts once) to run GP.

         :max-point-evaluations 10e100
          ;; The limit for the number of point (instruction) evaluations to
          ;; execute during the run.

         :genome-representation :plush
          ;; The representation for the genomes used for initialiation and inheritance.
          ;; Options include :plush and :plushy

         :max-points 200
          ;; Maximum size of push programs and push code, as counted by points
          ;; in the program. 1/4 this limit is used as the limit for sizes of
          ;; Plush genomes.

         :max-nested-depth 200
          ;; Maximum nested depth of push code and other nested objects.

         :max-genome-size-in-initial-program 50
          ;; Maximum size of initial Plush genomes in generation 0. Keep in mind
          ;; that genome lengths will otherwise be limited by 1/4 of :max-points.

         :evalpush-limit 150
          ;; The number of Push instructions that can be evaluated before stopping
          ;; evaluation.

         :evalpush-time-limit 0
          ;; The time in nanoseconds that a program can evaluate before stopping,
          ;; 0 means no time limit.

         :reuse-errors true
          ;; When true, children produced through direct reproduction will not be
          ;; re-evaluated but will have the error vector of their parent.

         :training-cases '()
          ;; The list of training cases (inputs and outputs). Used for some parent
          ;; selection methods, such as downsampled lexicase.

         :sub-training-cases '()
          ;; The subsample of the training cases used for downsampled lexicase.

         
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
                                          :uniform-addition-and-deletion 0.0
                                          :uniform-combination-and-deletion 0.0
                                          :genesis 0.0
                                          :gene-selection 0.0
                                          :uniform-reordering 0.0
                                          :uniform-segment-reordering 0.0
                                          :uniform-segment-transposition 0.0}
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

         :uniform-addition-and-deletion-rate 0.01
          ;; The probability, per gene, for additions in the first phase, and deletions in the second
          ;; phase (calculated for size-neutrality), of uniform-addition-and-deletion.

         :uniform-combination-rate 0.01
          ;; The probability, per gene, for combinations during uniform-combination

         :uniform-combination-and-deletion-rate 0.01
          ;; The probability, per gene, for combinations in the first phase, and deletions in
          ;; the second phase (calculated for size-neutrality), of uniform-combination-and-deletion.

         :uniform-silence-mutation-rate 0.1
          ;; The probability of each :silent being switched during uniform silent mutation.

         :uniform-reordering-rate 0.01
          ;; The probability that a pair of genes will be reordered with uninform-reordering,
          ;; or that a pair of segments will be transposed with uniform-segment-reordering.

         :uniform-segmenting-rate 0.01
          ;; The probability that segmenting for uniform-segment-transposition or 
          ;; uniform-segment-transposition will occur at each position in the genome.

         :uniform-transposition-rate 0.01
           ;; The probability that a segment will be transposed in uniform-segment-transposition.

         :uniform-segment-duplication-rate 0.01
           ;; The probability that a segment will be duplicated in uniform-segment-duplication.

         :uniform-segment-deletion-rate 0.01
           ;; The probability that a segment will be deleted in uniform-segment-deletion.

         :replace-child-that-exceeds-size-limit-with :random
          ;; When a child is produced that exceeds the size limit of (max-points / 4), this is
          ;; used to determine what program to return. Options include :parent, :empty, :random,
          ;; :truncate.

         :parent-reversion-probability 1.0
          ;; The probability of a child being reverted to its parent by a genetic operator that
          ;; has been made revertable, if the child is not as good as the parent on at least one
          ;; test case.

         :tag-enrichment 0
          ;; The number of extra copies of tag-related instructions that will be included in
          ;; the atom-generators.

         :tag-enrichment-types [:integer :boolean :exec :float :char :string :code]
          ;; The types for tag-related instructions that will be included in the atom-generators
          ;; when :tag-enrichment is greater than 0.

         :gene-selection-rate 1.0
          ;; When using gene-selection, the probability of selecting a new parent when moving
          ;; to the next gene.

         :autoconstructive false
          ;; If true, then :genetic-operator-probabilities will be {:autoconstruction 1.0},
          ;; :epigenetic-markers will be [:close :silent], and :atom-generators will include
          ;; instructions specified via :autoconstructive-genome-instructions, and other settings
          ;; specified in augment-for-autoconstruction. Also sets
          ;; :replace-child-that-exceeds-size-limit-with to :empty. Also, empty-genome individuals
          ;; will not be selected as parents. You will probably also want to provide a high value
          ;; for :max-generations. If :autoconstructive is :revertable, rather than true, then
          ;; :genetic-operator-probabilities will be {[:make-next-operator-revertable
          ;; :autoconstruction] 1.0}.

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

         :autoconstructive-clone-probability 0.0
          ;; Specifies the probability that a clone will be produced rather than the result of
          ;; actual autoconstruction, when :autoconstructive is true.

         :autoconstructive-decay 0.0
          ;; The rate for random gene deletions after autoconstruction.

         :autoconstructive-clone-decay :same
          ;; The rate for random gene deletions after cloning in autoconstruction. If this is
          ;; :same then the value for :autoconstructive-decay is used.

         :autoconstructive-parent-decay 0.0
          ;; The rate for random gene deletions in parent genomes used for autoconstruction.

         :autoconstructive-diffmeans-children 10
          ;; When using :autoconstructive-diversification-test :diffmeans-diversifying?, specifies
          ;; how many children of each child to generate and test. See genetic-operators.clj.

         :autoconstructive-si-children 8
          ;; When using a "size and instruction" diversification test, specifies how many
          ;; children to generate and test. See genetic-operators.clj.

         :autoconstructive-enough-new-errors-fraction 1/2
          ;; When using :enough-new-errors diversification test, specifies what fraction of the
          ;; errors in the maternal line must be distinct.

         :autoconstructive-integer-rand-enrichment 0
          ;; The number of extra instances of autoconstructive_integer_rand to include in
          ;; :atom-generators for autoconstruction. If negative then autoconstructive_integer_rand
          ;; will not be in :atom-generators at all.

         :autoconstructive-boolean-rand-enrichment 0
          ;; The number of extra instances of autoconstructive_boolean_rand to include in
          ;; :atom-generators for autoconstruction. If negative then autoconstructive_boolean_rand
          ;; will not be in :atom-generators at all.

         :autoconstructive-code-rand-atom-enrichment 0
          ;; The number of extra instances of autoconstructive_code_rand_atom to include in
          ;; :atom-generators for autoconstruction. If negative then autoconstructive_code_rand_atom
          ;; will not be in :atom-generators at all.

         :autoconstructive-tag-types [:integer :boolean :exec :float :char :string :code]
          ;; The types for tag-related instructions that will be included in the atom-generators
          ;; when :autoconstructive is true.

         :autoconstructive-environments false
          ;; If true, then :environment is included in the types for which instructions are
          ;; included for autoconstruction.

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
          ;; Arguments related to Plushy representaion
          ;;----------------------------------------

         :plushy-close-probability :automatic
          ;; Probability of choosing a :close (meta-) instruction when generating
          ;; Plushy genomes
          ;; Can either be a probability in [0, 1], or if :automatic, will be automatically
          ;; calculated based on the number of open parentheses generated by the instructions
          ;; in atom-generators

          ;;----------------------------------------
          ;; Arguments related to parent selection
          ;;----------------------------------------

         :parent-selection :lexicase
          ;; The parent selection method. Options include :tournament, :lexicase, :epsilon-lexicase,
          ;; :elitegroup-lexicase, :uniform, :leaky-lexicase, :random-threshold-lexicase,
          ;; :random-toggle-lexicase, :randomly-truncated-lexicase, :truncated-lexicase,
          ;; :novelty-search, :downsampled-lexicase

         :epsilon-lexicase-version :semi-dynamic
          ;; The version of epsilon-lexicase selection to use.
          ;; Options: :semi-dynamic (default and recommended), :dynamic, :static
         
         :epsilon-lexicase-epsilon nil
          ;; When parent-selection is :epsilon-lexicase,
          ;; the value for epsilon. If nil, automatic epsilon lexicase selection will be used.

         :epsilon-lexicase-probability 1
          ;; The probability that each filtering step in epsilon lexicase selection will allow
          ;; candidates with errors within epsilon of the best to survive, rather than just
          ;; the best.

         :random-threshold-lexicase-probability 1
          ;; The probability that each filtering step in random threshold lexicase selection will
          ;; allow candidates with errors equal to or better than a randomly chosen threshold to
          ;; survive, rather than just the best.

         :random-toggle-lexicase-probability 1
          ;; The probability that each filtering step in random toggle lexicase selection will
          ;; allow just the best to survive, rather than all individuals in the pool.

         :randomly-truncated-lexicase-probability 1
          ;; The probability that an application of randomly-truncated-lexicase-selection
          ;; will consider only a random subset of the test cases, rather than all of them.

         :truncated-lexicase-factor 0.1
          ;; When using truncated-lexicase for parent selection, gives the proportion
          ;; of thraining cases to use during selection.

         :lexicase-leakage 0.1
          ;; If using leaky lexicase selection, the probability that a selection event will return
          ;; a random (tourny 1) individual from the entire population.

         :lexicase-slippage 0
          ;; If using lexicase, leaky lexicase, epsilon lexicase, or random threshold lexicase
          ;; selection, the probability that each step of the lexicase selection process will
          ;; "slip" and return a random candidate from the current pool, rather than continuing
          ;; to filter the pool.

         :sort-meta-errors-for-lexicase :random
          ;; If using lexicase selection, determines how meta-errors will be sorted among
          ;; the actual errors. Options are :random (errors and meta-errors are shuffled
          ;; together), :first (meta-errors come first), or :last (meta-errors come last).

         :case-batch-size 1
         ;; When 1, does nothing.
         ;; When set to integer > 1, sets the batch size for batch lexicase selection.
         ;; Should work with any parent selection that uses an individual's :errors,
         ;; such as lexicase, epsilon-lexicase, etc.
         
         :tournament-size 7
          ;; If using tournament selection, the size of the tournaments.

         :total-error-method :sum
          ;; The method used to compute total error. Options include :sum (standard), :hah
          ;; (historically-assessed hardness), :rmse (root mean squared error), and :ifs
          ;; (implicit fitness sharing), :eliteness (count of non-elite errors)

         :normalization :none
          ;; The method used to normalize the errors to the range [0,1], with 0 being best. Options
          ;; include :none (no normalization), :divide-by-max-error (divides by value of argument
          ;; :max-error), :e-over-e-plus-1 (e/(e+1) = 1 - 1/(e+1))

         :max-error 1000
          ;; If :normalization is set to :divide-by-max-error, will use this number for
          ;; normalization.

         :meta-error-categories []
          ;; A vector containing meta-error categories that can be used for parent selection,
          ;; but that do not affect total error or the determination of whether an individual
          ;; is considered to be a solution. Each meta-error-category should either be a function
          ;; (which must be namespace-qualified if provided in a command-line argument) or a
          ;; keyword corresponding to a pre-defined meta-error function. In either case the
          ;; function should take an individual, an evaluated population, and an argmap, and
          ;; it should return a numeric meta error value or collection of values, for which
          ;; lower is interpreted as better. For keyword :foo, the corresponding meta-error
          ;; function will be clojush.meta-errors/foo-meta-error. See clojush.meta-errors for
          ;; the current options for pre-defined meta-error functions.

         :improvement-discount 0.5
          ;; The factor by successively older improvements are discounted when calculating
          ;; improvement-related meta-errors.

         :error-change-recency-limit 5
          ;; The number of generations within which an error change must have occurred to
          ;; have a :no-recent-error-change meta-error value of zero.

         :lineage-redundancy-window nil
          ;; If truthy, should be an integer which will be the number of history elements
          ;; used to calculate :lineage-redundancy meta-errors.

         :decimation-ratio 1 
          ;; If >= 1, does nothing. Otherwise, is the percent of the population
          ;; size that is retained before breeding. If 0 < decimation-ratio < 1, decimation
          ;; tournaments will be used to reduce the population to size (* population-size
          ;; decimation-ratio) before breeding.

         :decimation-tournament-size 2
          ;; Size of the decimation tournaments.

         :print-selection-counts false
          ;; If true, keeps track of and prints the number of times each individual was selected
          ;; to be a parent

         :print-preselection-fraction false
          ;; If true, keeps track of and prints the number of individuals that survive preselection
          ;; each generation. Does not take into account one-individual-per-error-vector-for-lexicase. 

         :self-mate-avoidance-limit 0
          ;; If non-zero, then when multiple parents are required for a genetic operator, an
          ;; effort will be made to select parents not equal to the first parent. The value
          ;; of this parameter is the number of re-selections that will be performed to try
          ;; to find a different parent, before using the same parent if the limit is exceeded.

         :age-mediated-parent-selection false
          ;; If truthy, should be a vector of [pmin pmax]. In this case, then with probability
          ;; pmin, parent selection will consider only individuals with the minimum age in
          ;; the population; with probability pmax, all individuals will be considered; with
          ;; probability (- 1.0 pmin pmax) an age cutoff will be chosen uniformly from
          ;; those present in the population, and only individuals with the cutoff age or
          ;; lower will be considered.
          ;;
          ;; NOTE: It doesn't make any sense to use this unless you have multiple ages in the
          ;; population, as you migh have, for example, from using the genesis operator or
          ;; autoconstruction.

         :age-combining-function :average
          ;; For genetic operators that involve multiple parents, the function used to combine
          ;; the incremented ages of the parents to produce the age of the child.

         :random-screen false
          ;; If truthy, should be a map with values for :criterion, :probability and possibly
          ;; :reversible. In this case, then with probability :probability, each parent
          ;; selection event will consider only individuals with :grain-size equal to or less
          ;; than a :grain-size chosen randomly from those present in the population. The
          ;; :criterion (see genetic-operators.clj for options) determines how :grain-size is
          ;; computed for an individual when it is created. If :reversible is truthy, then the
          ;; screen will be applied in reverse with probability 1/2, causing parent selection
          ;; to consider only individuals with :grain-size equal to or GREATER than the
          ;; chosen :grain-size.

         :knock-off-chip-off-the-old-block false
          ;; If truthy, then during preselection, if any individual has an error vector that
          ;; is different than its mother's, then do not allow any individual with errors
          ;; identical to its mother's to be selected. Requires :print-history to be true.
          ;; See preselection.clj for more options.

         :novelty-distance-metric :euclidean
          ;; When using novelty, the distance metric between two behavior vectors
          ;; Options: :manhattan, :euclidean

         :individuals-for-novelty-archive-per-generation 0
          ;; The number of individuals to add to the novelty archive per generation, if
          ;; using novelty search for parent selection or novelty as a meta-error. Default
          ;; of 0 means archive won't be maintained. Novelty GP paper uses one individual
          ;; every-other generation for 1000 generations; as such, a value of 1 seems like
          ;; a reasonable place to start.

         :novelty-number-of-neighbors-k 25
          ;; The number of neighbors to consider when calculating the sparseness with
          ;; regard to the nearest neighbors. Paper claims it is "robust to modest variation."

         :selection-delay false
          ;; If  this is truthy, then it should be a positive integer d, and all parents
          ;; will be selected with :uniform selection, but also, in each generation for
          ;; which (mod generation d) is 0, before producing offspring, the population
          ;; will be replaced with the results of repeated selection (using the specified
          ;; :parent-selection method) from an archive of all of the individuals that have
          ;; been produced since the previous time this was done.

         :preserve-frontier false
          ;; If truthy, then each child population will be replaced, after its errors have
          ;; been evaluated, with the product of a frontier-preservation process in which
          ;; individuals are repeatedly selected, without re-selection, from the concatenation
          ;; of the parent population with the collection of evaluated children. If the value
          ;; is :with-replacement, then individuals can be selected multiple times.

         :downsample-factor 1
          ;; Determines the proportion of cases to use when using downsampled lexicase.
          ;; When set to 1, has no effect. Should be in the range (0, 1].

         :use-ALPS false
          ;; When true, will enable the Age-Layered Population Structure. This is available
          ;; with any parent selection technique. With this implementation of ALPS, parents
          ;; for children of a given layer can be selected from that layer or any layer below
          ;; it. In other words, each layer has an age limit, and parents of individuals in
          ;; each layer can be of any age below the age limit.

         :ALPS-number-of-layers 10
          ;; The number of layers for the population when using ALPS

         :ALPS-age-limit-system :polynomial
          ;; The age limiting system used for ALPS.
          ;; Options: :polynomial, :linear, :exponential, :fibonacci
          ;; :polynomial, the default, is what is used in the first ALPS paper

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

         :lazy-automatic-simplification false
          ;; When true, uses lazy automatic simplification to only run the simplifying
          ;; program one input at a time until a non-zero error is found.

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

         :print-homology-data false
          ;; If true, prints the homology statistics.

         :print-lexicase-best-programs false
          ;; If true, prints the program with most elite cases and program with the
          ;; most zero errors when using lexicase selection.

         :exit-on-success true
          ;; When true, will exit the run when there is an individual with a zero-error vector

         :visualize false
          ;; When true, graphics will be displayed to indicate system progress.
          ;; See the comments in src/clojush/pushgp/visualize.clj

          ;;----------------------------------------
          ;; Arguments related to printing JSON, EDN, CSV, and remote recording
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
          ;; :plush-genome-size :push-program :plush-genome :genome-closes :push-paren-locations
          ;; :total-error :test-case-errors]

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

         :record-host nil
          ;; Should be in the format "<hostname>:<port>"
          ;; If set, will send logs of each run to a server running on this
          ;; host
         :label nil
          ;; If set, will send this in the configuration of the run, to the
          ;; external record
         
         :calculate-mod-metrics false
          ;; If true, will calculate modularity metrics (reuse and repetition) as the run proceeds.
          ;; By default, metrics are calculated on the execution trace for a randomly chosen test case.

          :simplification-steps-for-mod-metrics 0
          ;; Number of simplification steps applied to a program before calculating mod metrics.
          ;; 0 implies simplification won't be carried out.
          ;; WARNING: Keep this value low as every individual in the population will be simplified for this many number of steps
                
         )))

(defn augment-for-autoconstruction
  []
  (when (:autoconstructive @push-argmap)
    ;;
    ;; handle :revertable
    (if (= :revertable (:autoconstuctive @push-argmap))
      (swap! push-argmap assoc :genetic-operator-probabilities
             {[:make-next-operator-revertable :autoconstruction] 1.0})
      (swap! push-argmap assoc :genetic-operator-probabilities {:autoconstruction 1.0}))
    ;;
    ;; set :epigenetic-markers
    (swap! push-argmap assoc :epigenetic-markers [:close :silent])
    ;;
    ;; add autoconstructive-specific instructions (:genome or :gtm)
    (doseq [instr (case (:autoconstructive-genome-instructions @push-argmap)
                    :all (registered-for-stacks
                          (if (:autoconstructive-environments @push-argmap)
                            [:integer :boolean :exec :genome :float :tag :environment]
                            [:integer :boolean :exec :genome :float :tag]))
                    :gene-oriented (into (registered-for-stacks
                                          (if (:autoconstructive-environments @push-argmap)
                                            [:integer :boolean :exec :float :tag :environment]
                                            [:integer :boolean :exec :float :tag]))
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
                                           genome_instruction_eq
                                           genome_gene_close
                                           genome_gene_silent
                                           genome_close_inc
                                           genome_close_dec
                                           genome_new
                                           genome_parent1
                                           genome_parent2
                                           autoconstructive_integer_rand
                                           autoconstructive_boolean_rand
                                           genome_autoconstructing
                                           genome_if_autoconstructing
                                           genome_gene_genome_instruction
                                           genome_if_gene_genome_instruction
                                           genome_genesis))
                    :uniform (into (registered-for-stacks
                                    (if (:autoconstructive-environments @push-argmap)
                                      [:integer :boolean :exec :float :tag :environment]
                                      [:integer :boolean :exec :float :tag]))
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
                                     genome_autoconstructing
                                     genome_if_autoconstructing
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
                                     genome_uniform_addition_and_deletion
                                     genome_uniform_combination_and_deletion
                                     genome_genesis
                                     genome_alternation
                                     genome_uniform_crossover
                                     genome_gene_genome_instruction
                                     genome_if_gene_genome_instruction))
                    :non-recombinative (into (registered-for-stacks
                                              (if (:autoconstructive-environments @push-argmap)
                                                [:integer :boolean :exec :float :tag :environment]
                                                [:integer :boolean :exec :float :tag]))
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
                                                ;genome_gene_copy
                                                ;genome_gene_copy_range
                                               genome_toggle_silent
                                               genome_silence
                                               genome_unsilence
                                               genome_instruction_eq
                                               genome_gene_close
                                               genome_gene_silent
                                               genome_close_inc
                                               genome_close_dec
                                               genome_new
                                               genome_parent1
                                               genome_parent2
                                               autoconstructive_integer_rand
                                               autoconstructive_boolean_rand
                                               genome_autoconstructing
                                               genome_if_autoconstructing
                                               genome_gene_genome_instruction
                                               genome_if_gene_genome_instruction
                                               genome_genesis
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
                                               genome_uniform_addition_and_deletion
                                                ;genome_uniform_combination_and_deletion
                                                ;genome_alternation
                                                ;genome_uniform_crossover
                                               ))
                    :gene-oriented-non-recombinative
                    (into (registered-for-stacks
                           (if (:autoconstructive-environments @push-argmap)
                             [:integer :boolean :exec :float :tag :environment]
                             [:integer :boolean :exec :float :tag]))
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
                             ;genome_gene_copy
                             ;genome_gene_copy_range
                            genome_toggle_silent
                            genome_silence
                            genome_unsilence
                            genome_instruction_eq
                            genome_gene_close
                            genome_gene_silent
                            genome_close_inc
                            genome_close_dec
                            genome_new
                            genome_parent1
                            genome_parent2
                            autoconstructive_integer_rand
                            autoconstructive_boolean_rand
                            genome_autoconstructing
                            genome_if_autoconstructing
                            genome_gene_genome_instruction
                            genome_if_gene_genome_instruction
                            genome_genesis
                             ;;genome_uniform_instruction_mutation
                             ;;genome_uniform_integer_mutation
                             ;;genome_uniform_float_mutation
                             ;;genome_uniform_tag_mutation
                             ;;genome_uniform_string_mutation
                             ;;genome_uniform_boolean_mutation
                             ;;genome_uniform_close_mutation
                             ;;genome_uniform_silence_mutation
                             ;;genome_uniform_deletion
                             ;;genome_uniform_addition
                             ;;genome_uniform_addition_and_deletion
                             ;genome_uniform_combination_and_deletion
                             ;genome_alternation
                             ;genome_uniform_crossover
                            ))
                    :uniform-non-recombinative
                    (into (registered-for-stacks
                           (if (:autoconstructive-environments @push-argmap)
                             [:integer :boolean :exec :float :tag :environment]
                             [:integer :boolean :exec :float :tag]))
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
                             ;;genome_gene_dup
                             ;;genome_gene_randomize
                             ;;genome_gene_replace
                             ;;genome_gene_delete
                             ;;genome_rotate
                             ;genome_gene_copy
                             ;genome_gene_copy_range
                             ;;genome_toggle_silent
                             ;;genome_silence
                             ;;genome_unsilence
                             ;;genome_instruction_eq
                             ;;genome_gene_close
                             ;;genome_gene_silent
                             ;;genome_close_inc
                             ;;genome_close_dec
                            genome_new
                            genome_parent1
                            genome_parent2
                            autoconstructive_integer_rand
                            autoconstructive_boolean_rand
                            genome_autoconstructing
                            genome_if_autoconstructing
                            genome_gene_genome_instruction
                            genome_if_gene_genome_instruction
                            genome_genesis
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
                            genome_uniform_addition_and_deletion
                             ;genome_uniform_combination_and_deletion
                             ;genome_alternation
                             ;genome_uniform_crossover
                            ))
                    :gtm (let [by-type (registered-for-stacks
                                        (if (:autoconstructive-environments @push-argmap)
                                          [:integer :boolean :exec :float :tag :code :environment]
                                          [:integer :boolean :exec :float :tag :code]))]
                           (into by-type
                                 '(gtm_left
                                   gtm_right
                                   gtm_inc_delay
                                   gtm_dec_delay
                                   gtm_dub1
                                   gtm_dub2
                                   gtm_bounce1
                                   gtm_bounce2
                                   gtm_blank0
                                   gtm_blank1
                                   gtm_blank2
                                   gtm_erase
                                   gtm_read_all
                                   gtm_write_all
                                   gtm_read_instruction
                                   gtm_write_instruction
                                   gtm_read_silent
                                   gtm_write_silent
                                   gtm_read_close
                                   gtm_write_close
                                   autoconstructive_integer_rand
                                   autoconstructive_boolean_rand
                                   autoconstructive_code_rand_atom
                                   genome_autoconstructing
                                   genome_if_autoconstructing
                                   exec_k_when_autoconstructing
                                   exec_s_when_autoconstructing
                                   exec_y_when_autoconstructing)))
                    :appending (let [by-type (registered-for-stacks
                                              (if (:autoconstructive-environments @push-argmap)
                                                [:integer :boolean :exec :float :tag :code :environment]
                                                [:integer :boolean :exec :float :tag :code]))]
                                 (into by-type
                                       '(genome_append_parent1
                                         genome_append_parent2
                                         genome_append_random
                                         genome_length
                                         genome_parent1_length
                                         genome_parent2_length
                                         autoconstructive_integer_rand
                                         autoconstructive_boolean_rand
                                         genome_autoconstructing
                                         genome_if_autoconstructing)))
                    :appending1 (let [by-type (registered-for-stacks
                                               (if (:autoconstructive-environments @push-argmap)
                                                 [:integer :boolean :exec :float :tag :code :environment]
                                                 [:integer :boolean :exec :float :tag :code]))]
                                  (into by-type
                                        '(genome_append1_parent1
                                          genome_append1_parent2
                                          genome_append1_random
                                          genome_length
                                          genome_parent1_length
                                          genome_parent2_length
                                          autoconstructive_integer_rand
                                          autoconstructive_boolean_rand
                                          genome_autoconstructing
                                          genome_if_autoconstructing)))
                    :dubstep (let [by-type (registered-for-stacks
                                            (if (:autoconstructive-environments @push-argmap)
                                              [:integer :boolean :exec :float :tag :code :environment]
                                              [:integer :boolean :exec :float :tag :code]))]
                               (into by-type
                                     '(genome_dub1
                                       genome_dub2
                                       genome_step1
                                       genome_step2
                                       genome_back1
                                       genome_back2
                                       genome_append1_random
                                       genome_length
                                       genome_parent1_length
                                       genome_parent2_length
                                       autoconstructive_integer_rand
                                       autoconstructive_boolean_rand
                                       genome_autoconstructing
                                       genome_if_autoconstructing)))
                    :umad (into (registered-for-stacks
                                     (if (:autoconstructive-environments @push-argmap)
                                       [:integer :boolean :exec :float :tag :environment]
                                       [:integer :boolean :exec :float :tag]))
                                   '(genome_parent1
                                      genome_uniform_deletion
                                      genome_uniform_addition
                                      genome_uniform_addition_and_deletion)))]
      (when (not (some #{instr} (:atom-generators @push-argmap)))
        (swap! push-argmap assoc :atom-generators (conj (:atom-generators @push-argmap) instr))))
    ;;
    ;; include ERCs for floats, integers, and booleans
    (swap! push-argmap assoc
           :atom-generators (conj (:atom-generators @push-argmap)
                                  (fn [] (lrand))))
    (swap! push-argmap assoc
           :atom-generators (conj (:atom-generators @push-argmap)
                                  (fn [] (lrand-int 100))))
    (swap! push-argmap assoc
           :atom-generators (conj (:atom-generators @push-argmap)
                                  (fn [] (lrand-nth [true false]))))
    ;;
    ;; include instructions for using tags
    (swap! push-argmap assoc
           :atom-generators (conj (:atom-generators @push-argmap)
                                  (tag-instruction-erc
                                    (:autoconstructive-tag-types @push-argmap) 10000)))
    (swap! push-argmap assoc
           :atom-generators (conj (:atom-generators @push-argmap)
                                  (untag-instruction-erc 10000)))
    (swap! push-argmap assoc
           :atom-generators (conj (:atom-generators @push-argmap)
                                  (tagged-instruction-erc 10000)))
    ;;
    ;; enrich autoconstructive rand instructions
    (dotimes [n (:autoconstructive-integer-rand-enrichment @push-argmap)]
      (swap! push-argmap assoc
             :atom-generators (conj (:atom-generators @push-argmap)
                                    'autoconstructive_integer_rand)))
    (if (neg? (:autoconstructive-integer-rand-enrichment @push-argmap))
      (swap! push-argmap assoc :atom-generators (remove #(= % 'autoconstructive_integer_rand)
                                                        (:atom-generators @push-argmap))))
    (dotimes [n (:autoconstructive-boolean-rand-enrichment @push-argmap)]
      (swap! push-argmap assoc
             :atom-generators (conj (:atom-generators @push-argmap)
                                    'autoconstructive_boolean_rand)))
    (if (neg? (:autoconstructive-boolean-rand-enrichment @push-argmap))
      (swap! push-argmap assoc
             :atom-generators (remove #(= % 'autoconstructive_boolean_rand)
                                      (:atom-generators @push-argmap))))
    (dotimes [n (:autoconstructive-code-rand-atom-enrichment @push-argmap)]
      (swap! push-argmap assoc
             :atom-generators (conj (:atom-generators @push-argmap)
                                    'autoconstructive_code_rand_atom)))
    (if (neg? (:autoconstructive-code-rand-atom-enrichment @push-argmap))
      (swap! push-argmap assoc
             :atom-generators (remove #(= % 'autoconstructive_code_rand_atom)
                                      (:atom-generators @push-argmap))))
    ;;
    ;; specify that too-big children will be replaced with empty genomes
    (swap! push-argmap assoc
           :replace-child-that-exceeds-size-limit-with :empty)))

(defn load-push-argmap
  [argmap]
  (doseq [[argkey argval] argmap]
    (assert (contains? @push-argmap argkey)
            (str "Argument key " argkey " is not a recognized argument to pushgp."))
    (swap! push-argmap assoc argkey argval))
  (swap! push-argmap assoc :run-uuid (java.util.UUID/randomUUID))
  (augment-for-autoconstruction)
  (when (> (:tag-enrichment @push-argmap) 0)
    (let [types (:tag-enrichment-types @push-argmap)
          use-type #(some #{%} types)]
      (swap! push-argmap assoc
             :atom-generators
             (let [tag-instructions
                   (concat [(tag-instruction-erc types 10000)
                            (untag-instruction-erc 10000)
                            (tagged-instruction-erc 10000)
                            'integer_tagged_instruction]
                           (if (use-type :exec) '[integer_tag_exec_instruction] [])
                           (if (use-type :code) '[integer_tag_code_instruction] [])
                           (if (use-type :integer) '[integer_tag_integer_instruction] [])
                           (if (use-type :float) '[integer_tag_float_instruction] [])
                           (if (use-type :boolean) '[integer_tag_boolean_instruction] [])
                           (if (use-type :char) '[integer_tag_char_instruction] [])
                           (if (use-type :string) '[integer_tag_string_instruction] []))]
               (into (:atom-generators @push-argmap)
                     (take (* (:tag-enrichment @push-argmap) (count tag-instructions))
                           (cycle tag-instructions))))))))

(defn reset-globals
  "Resets all Clojush globals according to values in @push-argmap. If an argmap argument
  is provided then it is loaded into @push-argmap first."
  ([]
   (doseq [[gname gatom] (filter (fn [[a _]] (.startsWith (name a) "global-"))
                                 (ns-publics 'clojush.globals))]
     (if (contains? @push-argmap (keyword (.substring (name gname) (count "global-"))))
       (reset! @gatom (get @push-argmap (keyword (.substring (str gname) (count "global-")))))
       (throw (Exception. (str "globals.clj definition " gname
                               " has no matching argument in push-argmap. "
                               "Only such definitions should use the prefix 'global-'."))))))
  ([argmap]
   (load-push-argmap argmap)
   (reset-globals)))
