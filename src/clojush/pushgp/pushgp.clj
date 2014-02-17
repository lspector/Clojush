(ns clojush.pushgp.pushgp
  (:require [clojure.java.io :as io]
            [clj-random.core :as random])
  (:use [clojush globals util pushstate random individual evaluate]
        [clojush.instructions boolean code common numbers random-instructions string tag zip return]
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
          :initial-population nil ;; (MAY BE BROKEN) Can point to a file where an initial population is stored and can be read
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
          :population-size 1000
          :max-generations 1001
          :max-points 50 ;; Maximum size of push programs, as counted by points in the program
          :max-points-in-initial-program 50 ;; Maximum size of initial programs in generation 0
          :evalpush-limit 150 ;; The number of Push instructions that can be evaluated before stopping evaluation
          :evalpush-time-limit 0 ;; The time in nanoseconds that a program can evaluate before stopping, 0 means no time limit
          :reuse-errors true ;; When true, children produced through direct reproduction will not be re-evaluated but will have the error vector of their parent
          ;;
          ;;----------------------------------------
          ;; Genetic operator probabilities (replication-probability is 1.0 minus the rest)
          ;;----------------------------------------
          :reproduction-probability      0.1 ;; Direct reproduction, which makes a direct copy of the parent
          :mutation-probability          0.4 ;; Subtree mutation, similar to that found in tree-GP
          :crossover-probability         0.5 ;; Subtree crossover, similar to that found in tree-GP
          :simplification-probability    0.0 ;; Auto-simplification of the program
          :ultra-probability             0.0 ;; Uniform Linear Transformation with Repair and Alternation -- a uniform crossover and mutation operator
          :gaussian-mutation-probability 0.0 ;; Gaussian mutation affects only the float literals in a program by adding Gaussian noise
          :boolean-gsxover-probability   0.0 ;; Geometric Semantic Crossover -- creates random code that determines which of the two parents to use for each test cases
          :deletion-mutation-probability 0.0 ;; A mutation operator that deletes random instructions
          :parentheses-addition-mutation-probability 0.0 ;; Operator that randomly inserts a parentheses pair somewhere in the program
          :tagging-mutation-probability  0.0 ;; Operator that chooses a piece of code, moves it to beginning of program and tags it, and puts a tagged call in its place
          :tag-branch-mutation-probability 0.0 ;; Operator that inserts a tag-branch into the program. tag-branches compare two items on a stack and then jump to one of two tags depending on the comparison
          ;;
          ;;----------------------------------------
          ;; Arguments related to genetic operators
          ;;----------------------------------------
          :mutation-max-points 20 ;; The maximum number of points that new code will introduce during mutation
          :reproduction-simplifications 1 ;; The number of simplification steps that will happen during simplification reproduction
          :ultra-alternation-rate 0.1 ;; When using ULTRA, how often ULTRA alternates between the parents
          :ultra-alignment-deviation 1 ;; When using ULTRA, the standard deviation of how far alternation may jump between indices when switching between parents
          :ultra-mutation-rate 0.1 ;; The probability of each token being mutated during ULTRA
          :use-ultra-no-paren-mutation false ; If true, ULTRA will use no-paren mutation, which means that parentheses won't be added or deleted during mutation.
          :ultra-pads-with-empties false ;; If true then ULTRA pads the smaller parent with () and then removes them; if false then this is instead done using the symbol 'ultra-padding.
          :ultra-mutates-to-parentheses-frequently false ;; If true then when ULTRA mutates the new token will be "(" 1/3 of the time, ")" 1/3 of the time, and something else 1/3 of the time. When false, parentheses are treated like all other possible tokens.
          :gaussian-mutation-per-number-mutation-probability 0.5 ;; The probability that any given float literal will be affected by a pass of gaussian-mutate
          :gaussian-mutation-standard-deviation 0.1 ;; The standard deviation of a gaussian-mutated float
          :boolean-gsxover-new-code-max-points 20 ;; The maximum size of the random code fragment used in boolean-gsxover
          :tag-branch-mutation-type-instruction-pairs [] ;; A list of types and comparators that can be used by tag-branch-insertion-mutation
          ;;
          ;;----------------------------------------
          ;; Arguments related to node selection (used in mutate, crossover, and tagging-mutate)
          ;;----------------------------------------
          :node-selection-method :unbiased ;; The node selection method can be :unbiased, :leaf-probability, or :size-tournament
          :node-selection-leaf-probability 0.1 ;; If the node-selection-method is :leaf-probability, this is the percent of selections that will happen in leaves of the tree
          :node-selection-tournament-size 2 ;; If node-selection-method is :size-tournament, this is the size of the node selection tournaments
          ;;
          ;;----------------------------------------
          ;; Arguments related to parent selection
          ;;----------------------------------------
          :tournament-size 7 ;; If using tournament selection, the size of the tournaments
          :trivial-geography-radius 0 ;; If non-zero, this is used as the radius from which to select individuals for tournament or lexicase selection
          :decimation-ratio 1 ;; If >= 1, does nothing. Otherwise, is the percent of the population size that is retained before breeding. If 0 < decimation-ratio < 1, decimation tournaments will be used to reduce the population to size (* population-size decimation-ratio) before breeding. 
          :decimation-tournament-size 2 ;; Size of the decimation tournaments
          :use-historically-assessed-hardness false ;; When true, total error for tournament selection will depend on historically-assessed hardness
          :use-rmse false ;; When true, total error for tournament selection will depend on the root mean square error of the error vector
          :use-lexicase-selection false ;; If true, uses Lexicase Parent Selection (see Spector paper in GECCO-UP 2012 workshop proceedings)
          :use-elitegroup-lexicase-selection false ;; If true, uses elitegroup lexicase selection, an experimental change to lexicase that thus far is often worse
          ;;
          ;;----------------------------------------
          ;; Arguments related to the Push interpreter
          ;;----------------------------------------
          :pop-when-tagging true ;; When true, tagging instructions will pop the exec stack when tagging; otherwise, the exec stack is not popped
          :tag-limit 10000 ;; The size of the tag space
          :top-level-push-code true ;; When true, run-push will push the program's code onto the code stack prior to running
          :top-level-pop-code true ;; When true, run-push will pop the code stack after running the program
          ;;
          ;;----------------------------------------
          ;; Arguments related to generational and final reports
          ;;----------------------------------------
          :report-simplifications 100 ;; The number of simplification steps that will happen during report simplifications
          :final-report-simplifications 1000 ;; The number of simplification steps that will happen during final report simplifications
          :problem-specific-report default-problem-specific-report ;; A function can be called to provide a problem-specific report, which happens after the normal generational report is printed
          :print-errors true ;; When true, prints the error vector of the best individual
          :print-history false ;; When true, prints the history of the best individual's ancestors' total errors
          :print-timings false ; If true, report prints how long different parts of evolution have taken during the current run.
          :print-cosmos-data false ; If true, report prints COSMOS data each generation.
          :maintain-ancestors false  ; If true, save all ancestors in each individual (costly)
          :print-ancestors-of-solution false ; If true, final report prints the ancestors of the solution. Requires :maintain-ancestors to be true.
          :print-error-frequencies-by-case false ; If true, print reports of error frequencies by case each generation
          ;;
          ;;----------------------------------------
          ;; Arguments related to printing JSON or CSV logs
          ;;----------------------------------------
          :print-csv-logs false ;; Prints a CSV log of the population each generation
          :print-json-logs false ;; Prints a JSON log of the population each generation
          :csv-log-filename "log.csv" ;; The file to print CSV log to
          :json-log-filename "log.json" ;; The file to print JSON log to
          :log-fitnesses-for-all-cases false ;; If true, the CSV and JSON logs will include the fitnesses of each individual on every test case
          :json-log-program-strings false ;; If true, JSON logs will include program strings for each individual
          ;;
          ;;----------------------------------------
          ;; Other arguments
          ;;----------------------------------------
          :parent-reversion-probability 0.0 ;; The probability of a child being reverted to its parent if the parent has better fitness or equal fitness and is smaller
          :generate-bushy-random-code false ;; When true, random code will be "bushy", as in close to a binary tree
          )))

(defn load-push-argmap
  [argmap]
  (doseq [[argkey argval] argmap]
    (assert (contains? @push-argmap argkey) (str "Argument key " argkey " is not a recognized argument to pushgp."))
    (swap! push-argmap assoc argkey argval)))

(defn reset-globals
  []
  (doseq [[gname gatom] (filter (fn [[a _]] (.startsWith (name a) "global-")) (ns-publics 'clojush.globals))]
    (if (contains? @push-argmap (keyword (.substring (name gname) (count "global-"))))
      (reset! @gatom (get @push-argmap (keyword (.substring (str gname) (count "global-")))))
      (throw (Exception. (str "globals.clj definition " gname " has no matching argument in push-argmap. Only such definitions should use the prefix 'global-'."))))))

(defn make-agents-and-rng
  [{:keys [initial-population use-single-thread population-size
           max-points-in-initial-program atom-generators random-seed
           save-initial-population]}]
  (let [agent-error-handler (fn [agnt except]
                              (.printStackTrace except System/out)
                              (.printStackTrace except)
                              (System/exit 0))
        ;random-seeds (repeatedly population-size #(random/lrand-bytes (:mersennetwister random/*seed-length*)))];; doesn't ensure unique seeds
        random-seeds (loop [seeds '()]
                       (let [num-remaining (if initial-population
                                             (- (count initial-population) (count seeds))
                                             (- population-size (count seeds)))]
                         (if (pos? num-remaining)
                           (let [new-seeds (repeatedly num-remaining #(random/lrand-bytes (:mersennetwister random/*seed-length*)))]                             
                             (recur (concat seeds (filter (fn [candidate]
                                                            (not (some #(random/=byte-array % candidate)
                                                                       seeds))) new-seeds)))); only add seeds that we do not already have
                           seeds)))]
    {:pop-agents (if initial-population
                   (->> (read-string (slurp (str "data/" initial-population)))
                        (map #(if use-single-thread (atom %) (agent %)))
                        (vec))
                   (let [pa (doall (for [_ (range population-size)]
                                     (make-individual
                                       :program (random-code max-points-in-initial-program atom-generators))))
                         f (str "data/" (System/currentTimeMillis) ".ser")]
                     (when save-initial-population
                       (io/make-parents f)
                       (spit f (printable (map individual-string pa))))
                     (vec (map #(if use-single-thread (atom %) (agent %)) pa))))
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
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE ;might this need a dorun?


;; I feel like the printing should be in the main loop, but i'm just cutting and pasting for now
(defn parental-reversion
  [pop-agents generation {:keys [parent-reversion-probability use-single-thread use-rmse
                                 use-historically-assessed-hardness]}]
  (if (and (> generation 0) (> parent-reversion-probability 0))
    (let [err-fn (cond
                   use-historically-assessed-hardness :hah-error
                   use-rmse :rms-error
                   :else :total-error)]
      (println "Performing parent reversion...")
      (dorun (map #((if use-single-thread swap! send) 
                        % 
                        (fn [i]  
                          (if (or (< (err-fn i) (err-fn (:parent i)))
                                  (and (= (err-fn i) (err-fn (:parent i)))
                                       (< (count-points (:program i))
                                          (count-points (:program (:parent i)))))
                                  (> (lrand) parent-reversion-probability))
                            (assoc i :parent nil)  ;; don't store whole ancestry
                            (:parent i))))
                  pop-agents))
      (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
      (println "Done performing parent reversion."))))

(defn remove-parents
  "Removes value from :parent for each individual in the population. This will
   save memory."
  [pop-agents {:keys [use-single-thread]}]
  (dorun (map #((if use-single-thread swap! send)
                    %
                    (fn [i] (assoc i :parent nil)))
              pop-agents))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

;; this is a wrapper for calculate-hah-solution-rates, which should itself be changed
(defn calculate-hah-solution-rates-wrapper 
  [pop-agents {:keys [use-historically-assessed-hardness error-threshold population-size]}]
  (calculate-hah-solution-rates use-historically-assessed-hardness pop-agents error-threshold population-size))
          
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
  (let [prob-keywords (map keyword '(reproduction-probability mutation-probability crossover-probability simplification-probability
                                                              ultra-probability gaussian-mutation-probability boolean-gsxover-probability
                                                              deletion-mutation-probability parentheses-addition-mutation-probability
                                                              tagging-mutation-probability tag-branch-mutation-probability))
        prob-map (select-keys argmap prob-keywords)]
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
          (timer @push-argmap :reproduction)
          (print "Computing errors... ")
          (compute-errors pop-agents rand-gens @push-argmap)
          (println "Done computing errors.")
          (timer @push-argmap :fitness)
          ;; possible parent reversion
          (parental-reversion pop-agents generation @push-argmap)
          ;; stop tracking parents since they aren't used any more
          (remove-parents pop-agents @push-argmap)
          ;; calculate solution rates if necessary for historically-assessed hardness
          ;; change calculate-hah-solution-rates in the future, to destructure the argmap
          (calculate-hah-solution-rates-wrapper pop-agents @push-argmap)
          ;; create global structure to support elitegroup lexicase selection
          (when (:use-elitegroup-lexicase-selection @push-argmap)
            (build-elitegroups pop-agents))
          (timer @push-argmap :other)
          ;; report and check for success
          (let [outcome (report-and-check-for-success (vec (doall (map deref pop-agents)))
                                                      generation @push-argmap)]
            (cond (= outcome :failure) (do (printf "\nFAILURE\n") (flush))
                  (= outcome :continue) (do (timer @push-argmap :report)
                                            (println "\nProducing offspring...")
                                            (produce-new-offspring pop-agents child-agents rand-gens @push-argmap)
                                            (println "Installing next generation...")
                                            (install-next-generation pop-agents child-agents @push-argmap)
                                            (recur (inc generation)))
                  :else (final-report generation outcome @push-argmap))))))))
