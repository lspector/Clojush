(ns clojush.pushgp.pushgp
  (:require [clojure.java.io :as io]
            [clj-random.core :as random]
            [clojure.repl :as repl])
  (:use [clojush globals util pushstate random individual evaluate simplification translate]
        [clojush.instructions boolean code common numbers random-instructions string char vectors tag zip return input-output genome]
        [clojush.pushgp breed parent-selection report]
        [clojush.experimental.decimation]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushgp

(def push-argmap
  (atom (sorted-map
          ;;----------------------------------------
          ;; Clojush system arguments
          ;;----------------------------------------
          :use-single-thread false ;; When true, Clojush will only use a single thread
          :random-seed (random/generate-mersennetwister-seed) ;; The seed for the random number generator
          :save-initial-population false ;; When true, saves the initial population
          ;;
          ;;----------------------------------------
          ;; Standard GP arguments
          ;;----------------------------------------
          :error-function (fn [p] '(0)) ;; Function that takes a program and returns a list of errors
          :error-threshold 0 ;; Pushgp will stop and return the best program if its total error is <= the error-threshold
          :atom-generators (concat @registered-instructions ;; The instructions that pushgp will use in random code
                                   (list 
                                     (fn [] (lrand-int 100))
                                     (fn [] (lrand))))
          :population-size 1000 ;; Number of individuals in the population
          :max-generations 1001 ;; The maximum number of generations to run GP
          :max-point-evaluations 10e100 ;; The limit for the number of point (instruction) evaluations to execute during the run
          :max-points 200 ;; Maximum size of push programs and push code, as counted by points in the program. 1/4 this limit is used as the limit for sizes of Plush genomes.
          :max-genome-size-in-initial-program 50 ;; Maximum size of initial Plush genomes in generation 0. Keep in mind that genome lengths will otherwise be limited by 1/4 of :max-points
          :evalpush-limit 150 ;; The number of Push instructions that can be evaluated before stopping evaluation
          :evalpush-time-limit 0 ;; The time in nanoseconds that a program can evaluate before stopping, 0 means no time limit
          :reuse-errors true ;; When true, children produced through direct reproduction will not be re-evaluated but will have the error vector of their parent
          :pass-individual-to-error-function false ;; When true, entire individuals (rather than just programs) are passed to error functions
          ;;
          ;;----------------------------------------
          ;; Genetic operator probabilities
          ;;----------------------------------------
          ;; The map supplied to :genetic-operator-probabilities should contain genetic operators
          ;; that sum to 1.0. All available genetic operators are defined in clojush.pushgp.breed.
          ;; Along with single operators, pipelines (vectors) containing multiple operators are
          ;; also allowed, where each operator is applied to the child of the previous operator, along
          ;; with newly selecting individuals where necessary. If an operator is preceeded by
          ;; :make-next-operator-revertable, it will only keep the child if it is at least as good as
          ;; its (first) parent on every test case.
          :genetic-operator-probabilities {:reproduction 0.0
                                           :alternation 0.7
                                           :uniform-mutation 0.1
                                           [:alternation :uniform-mutation] 0.2 ;Somewhat equivalent to normal Push's ULTRA operator
                                           :uniform-close-mutation 0.0
                                           :uniform-silence-mutation 0.0
                                           [:make-next-operator-revertable :uniform-silence-mutation] 0.0 ;Equivalent to a hill-climbing version of uniform-silence-mutation
                                           :autoconstruction 0.0
                                           :uniform-deletion 0.0
                                           }
          ;;
          ;;----------------------------------------
          ;; Arguments related to genetic operators
          ;;----------------------------------------
          :alternation-rate 0.01 ;; When using alternation, how often alternates between the parents
          :alignment-deviation 10 ;; When using alternation, the standard deviation of how far alternation may jump between indices when switching between parents
          :uniform-mutation-rate 0.01 ;; The probability of each token being mutated during uniform mutation
          :uniform-mutation-constant-tweak-rate 0.5 ;; The probability of using a constant mutation instead of simply replacing the token with a random instruction during uniform mutation
          :uniform-mutation-float-gaussian-standard-deviation 1.0 ;; The standard deviation used when tweaking float constants with Gaussian noise
          :uniform-mutation-int-gaussian-standard-deviation 1 ;; The standard deviation used when tweaking integer constants with Gaussian noise
          :uniform-mutation-string-char-change-rate 0.1 ;; The probability of each character being changed when doing string constant tweaking
          :uniform-mutation-tag-gaussian-standard-deviation 100 ;; The standard deviation used when tweaking tag locations with Gaussian noise
          :uniform-close-mutation-rate 0.1 ;; The probability of each :close being incremented or decremented during uniform close mutation
          :close-increment-rate 0.2 ;; The probability of making an increment change to :close during uniform close mutation, as opposed to a decrement change
          :uniform-deletion-rate 0.01 ;; The probability that any instruction will be deleted during uniform deletion
          :uniform-silence-mutation-rate 0.1 ;; The probability of each :silent being switched during uniform silent mutation
          :replace-child-that-exceeds-size-limit-with :random ;; When a child is produced that exceeds the size limit of (max-points / 4), this is used to determine what program to return. Options include :parent, :empty, :random, :truncate
          :parent-reversion-probability 1.0 ;; The probability of a child being reverted to its parent by a genetic operator that has been made revertable, if the child is not as good as the parent on at least one test case
          :autoconstructive false ;; if true then :genetic-operator-probabilities will be {:autoconstruction 1.0}, :epigenetic-markers will be [:close :silent], and :atom-generators will include everything in (registered-for-stacks [:integer :boolean :exec :genome]). Also sets :replace-child-that-exceeds-size-limit-with to :empty. You will probably also want to provide a high value for :max-generations.
          :autoconstructive-integer-rand-enrichment 1 ;; the number of extra instances of autoconstructive_integer_rand to include in :atom-generators for autoconstruction. If negative then autoconstructive_integer_rand will not be in :atom-generators at all
          :autoconstructive-boolean-rand-enrichment -1 ;; the number of extra instances of autoconstructive_boolean_rand to include in :atom-generators for autoconstruction. If negative then autoconstructive_boolean_rand will not be in :atom-generators at all
          ;;
          ;;----------------------------------------
          ;; Epignenetics
          ;;----------------------------------------
          :epigenetic-markers [:close] ;; A vector of the epigenetic markers that should be used in the individuals. Implemented options include: :close, :silent
          :close-parens-probabilities [0.772 0.206 0.021 0.001] ;; A vector of the probabilities for the number of parens ending at that position. See random-closes in clojush.random          
          :silent-instruction-probability 0.2 ;; If :silent is used as an epigenetic-marker, this is the probability of random instructions having :silent be true
          ;;
          ;;----------------------------------------
          ;; Arguments related to parent selection
          ;;----------------------------------------
          :parent-selection :lexicase ;; The parent selection method. Options include :tournament, :lexicase, :elitegroup-lexicase, :uniform :leaky-lexicase
          :lexicase-leakage 0.1 ;; If using leaky lexicase selection, the percentage of selection events that will return random (tourny 1) individuals
          :tournament-size 7 ;; If using tournament selection, the size of the tournaments
          :total-error-method :sum ;; The method used to compute total error. Options include :sum (standard), :hah (historically-assessed hardness), :rmse (root mean squared error), and :ifs (implicit fitness sharing)
          :normalization :none ;; The method used to normalize the errors to the range [0,1], with 0 being best. Options include :none (no normalization), :divide-by-max-error (divides by value of argument :max-error), :e-over-e-plus-1 (e/(e+1) = 1 - 1/(e+1))
          :max-error 1000 ;; If :normalization is set to :max-error, will use this number for normalization
          :meta-error-categories [] ;; A vector containing meta-error categories that can be used for parent selection, but do not affect total error. See clojush.evaluate for options.
          :trivial-geography-radius 0 ;; If non-zero, this is used as the radius from which to select individuals for tournament or lexicase selection
          :decimation-ratio 1 ;; If >= 1, does nothing. Otherwise, is the percent of the population size that is retained before breeding. If 0 < decimation-ratio < 1, decimation tournaments will be used to reduce the population to size (* population-size decimation-ratio) before breeding.
          :decimation-tournament-size 2 ;; Size of the decimation tournaments
          :print-selection-counts false ;; If true, keeps track of and prints the number of times each individual was selected to be a parent
          ;;
          ;;----------------------------------------
          ;; Arguments related to the Push interpreter
          ;;----------------------------------------
          :pop-when-tagging true ;; When true, tagging instructions will pop the exec stack when tagging; otherwise, the exec stack is not popped
          :tag-limit 10000 ;; The size of the tag space
          :top-level-push-code false ;; When true, run-push will push the program's code onto the code stack prior to running
          :top-level-pop-code false ;; When true, run-push will pop the code stack after running the program
          ;;
          ;;----------------------------------------
          ;; Arguments related to generational and final reports
          ;;----------------------------------------
          :report-simplifications 100 ;; The number of simplification steps that will happen during report simplifications
          :final-report-simplifications 1000 ;; The number of simplification steps that will happen during final report simplifications
          :problem-specific-report default-problem-specific-report ;; A function can be called to provide a problem-specific report, which happens after the normal generational report is printed
          :return-simplified-on-failure false ;; When true, will simplify the best indivual and return it, even if the error threshold has not been reached. This will make failures return the same as successes
          :print-errors true ;; When true, prints the error vector of the best individual
          :print-history false ;; When true, prints the history of the best individual's ancestors' total errors
          :print-timings false ; If true, report prints how long different parts of evolution have taken during the current run.
          :print-error-frequencies-by-case false ; If true, print reports of error frequencies by case each generation
          :print-cosmos-data false ; If true, report prints COSMOS data each generation.
          :maintain-ancestors false  ; If true, save all ancestors in each individual (costly)
          :print-ancestors-of-solution false ; If true, final report prints the ancestors of the solution. Requires :maintain-ancestors to be true.
          :print-behavioral-diversity false ; If true, prints the behavioral diversity of the population each generation. Note: The error function for the problem must support behavioral diversity. For an example, see wc.clj
          :print-homology-data false ; If true, prints the homology statistics
          ;;
          ;;----------------------------------------
          ;; Arguments related to printing JSON or CSV logs
          ;;----------------------------------------
          :print-csv-logs false ;; Prints a CSV log of the population each generation
          :print-json-logs false ;; Prints a JSON log of the population each generation
          :csv-log-filename "log.csv" ;; The file to print CSV log to
          :json-log-filename "log.json" ;; The file to print JSON log to
          :csv-columns [:generation :location :total-error :push-program-size] ;; The columns to include in a printed CSV beyond the generation and individual. Options include: [:generation :location :parent-uuids :genetic-operators :push-program-size :plush-genome-size :push-program :plush-genome :total-error :test-case-errors]
          :log-fitnesses-for-all-cases false ;; If true, the CSV and JSON logs will include the fitnesses of each individual on every test case
          :json-log-program-strings false ;; If true, JSON logs will include program strings for each individual
          )))

(defn load-push-argmap
  [argmap]
  (doseq [[argkey argval] argmap]
    (assert (contains? @push-argmap argkey) (str "Argument key " argkey " is not a recognized argument to pushgp."))
    (swap! push-argmap assoc argkey argval))
  (when (:autoconstructive @push-argmap)
    (swap! push-argmap assoc :genetic-operator-probabilities {:autoconstruction 1.0})
    (swap! push-argmap assoc :epigenetic-markers [:close :silent])
    (doseq [instr (registered-for-stacks [:integer :boolean :exec :genome])]
      (when (not (some #{instr} (:atom-generators @push-argmap)))
        (swap! push-argmap assoc :atom-generators (conj (:atom-generators @push-argmap) instr))))
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

(defn make-agents-and-rng
  [{:keys [use-single-thread population-size random-seed
           max-genome-size-in-initial-program atom-generators
           save-initial-population]
    :as argmap}]
  (let [agent-error-handler (fn [agnt except]
                              ;(.printStackTrace except System/out)
                              ;(.printStackTrace except)
                              (repl/pst except 10000)
                              (System/exit 0))
        random-seeds (loop [seeds '()]
                       (let [num-remaining (- population-size (count seeds))]
                         (if (pos? num-remaining)
                           (let [new-seeds (repeatedly num-remaining #(random/lrand-bytes (:mersennetwister random/*seed-length*)))]                             
                             (recur (concat seeds (filter (fn [candidate]
                                                            (not (some #(random/=byte-array % candidate)
                                                                       seeds))) new-seeds)))); only add seeds that we do not already have
                           seeds)))]
    {:pop-agents (let [pa (doall (for [_ (range population-size)]
                                   (make-individual
                                     :genome (random-plush-genome max-genome-size-in-initial-program
                                                                  atom-generators
                                                                  argmap)
                                     :genetic-operators :random)))
                       f (str "data/" (System/currentTimeMillis) ".ser")]
                   (when save-initial-population
                     (io/make-parents f)
                     (spit f (printable (map individual-string pa))))
                   (vec (map #(if use-single-thread
                                (atom %)
                                (agent % :error-handler agent-error-handler))
                             pa)))
     :child-agents (vec (doall (for [_ (range population-size)]
                                 ((if use-single-thread atom agent)
                                      (make-individual)
                                      :error-handler agent-error-handler))))
     :random-seeds random-seeds
     :rand-gens (vec (doall (for [k (range population-size)]                      
                              (random/make-mersennetwister-rng (nth random-seeds k)))))
     }))

(defn compute-errors
  [pop-agents rand-gens {:keys [use-single-thread error-function] :as argmap}]
  (dorun (map #((if use-single-thread swap! send)
                    % evaluate-individual error-function %2 argmap)
              pop-agents
              rand-gens))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(defn produce-new-offspring
  [pop-agents child-agents rand-gens
   {:keys [decimation-ratio population-size decimation-tournament-size
           trivial-geography-radius use-single-thread ]}]
  (let [pop (if (>= decimation-ratio 1)
              (vec (doall (map deref pop-agents)))
              (decimate (vec (doall (map deref pop-agents)))
                        (int (* decimation-ratio population-size))
                        decimation-tournament-size
                        trivial-geography-radius))]
    (dotimes [i population-size]
      ((if use-single-thread swap! send)
           (nth child-agents i) 
           breed 
           i (nth rand-gens i) pop @push-argmap)))
  (when-not use-single-thread (apply await child-agents))) ;; SYNCHRONIZE

(defn install-next-generation
  [pop-agents child-agents {:keys [population-size use-single-thread]}]
  (dotimes [i population-size]
    ((if use-single-thread swap! send)
         (nth pop-agents i) (fn [av] (deref (nth child-agents i)))))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(defn check-genetic-operator-probabilities-add-to-one
  [argmap]
  (let [prob-map (:genetic-operator-probabilities argmap)]
    (doseq [gen-op (keys prob-map)]
      (if (sequential? gen-op)
        (doseq [g gen-op]
          (assert (contains? genetic-operators g)
                  (str "Unrecognized genetic operator: " g " in " gen-op)))
        (assert (contains? genetic-operators gen-op)
                (str "Unrecognized genetic operator: " gen-op))))
    (assert (< 0.99999
               (apply + (vals prob-map))
               1.00001)
            (str "Genetic operator probabilities do not sum to 1.0:\n"
                 (clojure.string/replace (str prob-map \newline)
                                         \,
                                         \newline)))))

(defn timer
  "Used to track the time used by different parts of evolution."
  [{:keys [print-timings]} step]
  (when print-timings
    (let [start-time @timer-atom
          current-time-for-step (get @timing-map step)]
      (reset! timer-atom (System/currentTimeMillis))
      (swap! timing-map assoc step (+ current-time-for-step (- @timer-atom start-time))))))

(defn pushgp
  "The top-level routine of pushgp."
  ([] (pushgp '()))
  ([args]
    (reset! timer-atom (System/currentTimeMillis))
    (load-push-argmap args)
    (random/with-rng (random/make-mersennetwister-rng (:random-seed @push-argmap))
      ;; set globals from parameters
      (reset-globals)
      (initial-report) ;; Print the inital report
      (print-params @push-argmap)
      (check-genetic-operator-probabilities-add-to-one @push-argmap)
      (timer @push-argmap :initialization)
      (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      (println "\nGenerating initial population...")
      (let [{:keys [pop-agents child-agents rand-gens random-seeds]} (make-agents-and-rng @push-argmap)]
        ;(print "Random seeds: ")
        ;(doseq [seed random-seeds] (print " " seed))
        ;(println)
        ;; Main loop
        (loop [generation 0]
          (println "Processing generation:" generation)
          (population-translate-plush-to-push pop-agents @push-argmap)
          (timer @push-argmap :reproduction)
          (print "Computing errors... ")
          (compute-errors pop-agents rand-gens @push-argmap)
          (println "Done computing errors.")
          (timer @push-argmap :fitness)
          ;; calculate solution rates if necessary for historically-assessed hardness
          (calculate-hah-solution-rates pop-agents @push-argmap)
          ;; create global structure to support elite group lexicase selection
          (when (= (:parent-selection @push-argmap) :elitegroup-lexicase)
            (build-elitegroups pop-agents))
          ;; calculate implicit fitness sharing fitness for population
          (when (= (:total-error-method @push-argmap) :ifs)
            (calculate-implicit-fitness-sharing pop-agents @push-argmap))
          (timer @push-argmap :other)
          ;; report and check for success
          (let [[outcome best] (report-and-check-for-success (vec (doall (map deref pop-agents)))
                                                             generation @push-argmap)]
            (cond (= outcome :failure) (do (printf "\nFAILURE\n")
                                         (if (:return-simplified-on-failure @push-argmap)
                                           (auto-simplify best (:error-function @push-argmap) (:final-report-simplifications @push-argmap) true 500)
                                           (flush)))
                  (= outcome :continue) (do (timer @push-argmap :report)
                                          (println "\nProducing offspring...")
                                          (produce-new-offspring pop-agents child-agents rand-gens @push-argmap)
                                          (println "Installing next generation...")
                                          (install-next-generation pop-agents child-agents @push-argmap)
                                          (recur (inc generation)))
                  :else  (final-report generation best @push-argmap))))))))
