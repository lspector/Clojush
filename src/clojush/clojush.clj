;; clojush.clj
;;
;; This file implements a version of the Push programming language and the PushGP genetic
;; programming system in the Clojure programming language. See the accompanying README
;; file for usage instructions and other notes.
;;
;; Copyright (c) 2010 Lee Spector (lspector@hampshire.edu)
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of version 3 of the GNU General Public License as published by the
;; Free Software Foundation, available from http://www.gnu.org/licenses/gpl.txt.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE. See the GNU General Public License (http://www.gnu.org/licenses/)
;; for more details.

;;;;;
;; namespace declaration and access to needed libraries
(ns clojush.clojush
  (:gen-class)
  (:require
    [clojure.math.numeric-tower :as math]
    [clojure.string :as string]
    [local-file])
  (:use
    [clojush.globals]
    [clojush.random]
    [clojush.util]
    [clojush.pushstate]
    [clojush.instructions.boolean]
    [clojush.instructions.code]
    [clojush.instructions.common]
    [clojush.instructions.numbers]
    [clojush.instructions.random]
    [clojush.instructions.string]
    [clojush.instructions.tag]
    [clojush.instructions.zip]
    [clojush.experimental.tagged_code_macros]))

(import java.lang.Math)

;; backtrace abbreviation, to ease debugging
(defn bt []
  (.printStackTrace *e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print all registered instructions on loading

(printf "\nRegistered instructions: %s\n\n" @registered-instructions)
(flush)

;; also set default value for atom-generators
(reset! global-atom-generators 
        (concat @registered-instructions
                (list 
                  (fn [] (lrand-int 100))
                  (fn [] (lrand)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push interpreter

(defn recognize-literal
  "If thing is a literal, return its type -- otherwise return false."
  [thing]
  (cond (integer? thing) :integer
        (number? thing) :float
        (string? thing) :string
        (or (= thing true) (= thing false)) :boolean
        true false))

(def debug-recent-instructions ())

(defn execute-instruction
  "Executes a single Push instruction."
  [instruction state]
  ;; for debugging only, e.g. for stress-test
  ;(def debug-recent-instructions (cons instruction debug-recent-instructions))
  ;(def debug-recent-state state)
  (if (= instruction nil) ;; tests for nil and ignores it
    state
    (let [literal-type (recognize-literal instruction)]
      (cond 
        literal-type (push-item instruction literal-type state)
        (tag-instruction? instruction) (handle-tag-instruction instruction state)
        (tagged-code-macro? instruction) (handle-tag-code-macro instruction state)
        :else ((instruction @instruction-table) state)))))


(defn eval-push 
  "Executes the contents of the exec stack, aborting prematurely if execution limits are 
exceeded. The resulting push state will map :termination to :normal if termination was 
normal, or :abnormal otherwise."
  ([state] (eval-push state false false))
  ([state print] (eval-push state print false))
  ([state print trace]
    (loop [iteration 1 s state
           time-limit (if (zero? @global-evalpush-time-limit)
                        0
                        (+' @global-evalpush-time-limit (System/nanoTime)))]
      (if (or (> iteration @global-evalpush-limit)
            (empty? (:exec s))
            (and (not (zero? time-limit))
              (> (System/nanoTime) time-limit)))
        (assoc s :termination (if (empty? (:exec s)) :normal :abnormal))
        (let [exec-top (top-item :exec s)
              s (pop-item :exec s)]
          (let [s (if (seq? exec-top)
                    (assoc s :exec (concat exec-top (:exec s)))
                    (let [execution-result (execute-instruction exec-top s)]
                      (cond 
                        (= trace false) execution-result
                        (= trace true) (assoc execution-result
                                         :trace
                                         (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))
                        (= trace :changes) (if (= execution-result s)
                                             execution-result
                                             (assoc execution-result
                                               :trace
                                               (cons exec-top (let [t (:trace s)] (if (seq? t) t ()))))))))]
            (when print
              (printf "\nState after %s steps (last step: %s):\n" 
                iteration (if (seq? exec-top) "(...)" exec-top))
              (state-pretty-print s))
            (recur (inc iteration) s time-limit)))))))

(defn run-push 
  "The top level of the push interpreter; calls eval-schush between appropriate code/exec 
pushing/popping. The resulting push state will map :termination to :normal if termination was 
normal, or :abnormal otherwise."
  ([code state]
    (run-push code state false false))
  ([code state print]
    (run-push code state print false))
  ([code state print trace]
    (let [s (if top-level-push-code (push-item code :code state) state)]
      (let [s (push-item code :exec s)]
        (when print
          (printf "\nState after 0 steps:\n")
          (state-pretty-print s))
        (let [s (eval-push s print trace)]
          (if top-level-pop-code
            (pop-item :code s)
            s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushgp

;; Individuals are records.
;; Populations are vectors of agents with individuals as their states (along with error and
;; history information).


(defrecord individual [program errors total-error hah-error history ancestors])

(defn make-individual [& {:keys [program errors total-error hah-error history ancestors]
                          :or {program nil
                               errors nil
                               total-error nil ;; a non-number is used to indicate no value
                               hah-error nil
                               history nil
                               ancestors nil}}]
  (individual. program errors total-error hah-error history ancestors))

(defn compute-total-error
  [errors]
  (reduce +' errors))

(defn compute-hah-error
  [errors]
  (if @global-use-historically-assessed-hardness
    (reduce +' (doall (map (fn [rate e] (*' (- 1.01 rate) e))
                           @solution-rates
                           errors)))
    nil))

(defn choose-node-index-with-leaf-probability
  "Returns an index into tree, choosing a leaf with probability 
   @global-node-selection-leaf-probability."
  [tree]
  (if (seq? tree)
    (if (> (lrand) @global-node-selection-leaf-probability)
      (second (lrand-nth (filter #(seq? (first %)) (map #(list %1 %2) (all-items tree) (iterate inc 0)))))
      (let [indexed-leaves (filter #(not (seq? (first %))) (map #(list %1 %2) (all-items tree) (iterate inc 0)))]
        (if (empty? indexed-leaves) 0 (second (lrand-nth indexed-leaves)))))
    0))

(defn choose-node-index-by-tournament
  "Returns an index into tree, choosing the largest subtree found in 
   a tournament of size @global-node-selection-tournament-size."
  [tree]
  (let [c (count-points tree)
        tournament-set
        (for [_ (range @global-node-selection-tournament-size)]
          (let [point-index (lrand-int c)
                subtree-size (count-points (code-at-point tree point-index))]
            {:i point-index :size subtree-size}))]
    (:i (last (sort-by :size tournament-set)))))

(defn select-node-index
  "Returns an index into tree using the node selection method indicated
   by @global-node-selection-method."
  [tree]
  (let [method @global-node-selection-method]
    (cond 
      (= method :unbiased) (lrand-int (count-points tree))
      (= method :leaf-probability) (choose-node-index-with-leaf-probability tree)
      (= method :size-tournament) (choose-node-index-by-tournament tree))))

(defn flatten-seqs
  "A version of flatten that only flattens nested seqs."
  [x]
  (filter (complement seq?)
          (rest (tree-seq seq? seq x))))

(defn auto-simplify 
  "Auto-simplifies the provided individual."
  [ind error-function steps print? progress-interval]
  (when print? (printf "\nAuto-simplifying with starting size: %s" (count-points (:program ind))))
  (loop [step 0 program (:program ind) errors (:errors ind) total-errors (:total-error ind)]
    (when (and print? 
               (or (>= step steps)
                   (zero? (mod step progress-interval))))
      (printf "\nstep: %s\nprogram: %s\nerrors: %s\ntotal: %s\nsize: %s\n" 
              step (not-lazy program) (not-lazy errors) total-errors (count-points program))
      (flush))
    (if (>= step steps)
      (make-individual :program program :errors errors :total-error total-errors 
                       :history (:history ind) 
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind)))
      (let [new-program (if (< (lrand-int 5) 4)
                          ;; remove a small number of random things
                          (loop [p program how-many (inc (lrand-int 2))]
                            (if (zero? how-many)
                              p
                              (recur (remove-code-at-point p (lrand-int (count-points p)))
                                     (dec how-many))))
                          ;; flatten something
                          (let [point-index (lrand-int (count-points program))
                                point (code-at-point program point-index)]
                            (if (seq? point)
                              (insert-code-at-point program point-index (flatten-seqs point))
                              program)))
            new-errors (error-function new-program)
            new-total-errors (compute-total-error new-errors)]
        (if (<= new-total-errors total-errors)
          (recur (inc step) new-program new-errors new-total-errors)
          (recur (inc step) program errors total-errors))))))

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

(defn lexicase-selection
  "Returns an individual that does the best on a randomly selected set of fitness cases"
  [pop]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (first survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn select
  "Returns a selected parent, using lexicase or tournament selection."
  [pop tournament-size radius location]
  (if @global-use-lexicase-selection
    (lexicase-selection pop)
    (let [tournament-set 
          (doall
            (for [_ (range tournament-size)]
              (nth pop
                   (if (zero? radius)
                     (lrand-int (count pop))
                     (mod (+ location (- (lrand-int (+ 1 (* radius 2))) radius))
                          (count pop))))))
          err-fn (if @global-use-historically-assessed-hardness :hah-error :total-error)]
      (reduce (fn [i1 i2] (if (< (err-fn i1) (err-fn i2)) i1 i2))
              tournament-set))))

(defn mutate 
  "Returns a mutated version of the given individual."
  [ind mutation-max-points max-points atom-generators]
  (let [new-program (insert-code-at-point (:program ind) 
                                          (select-node-index (:program ind))
                                          (random-code mutation-max-points atom-generators))]
    (if (> (count-points new-program) max-points)
      ind
      (make-individual :program new-program :history (:history ind)
                       :ancestors (if maintain-ancestors
                                    (cons (:program ind) (:ancestors ind))
                                    (:ancestors ind))))))

;; some utilities are required for gaussian mutation

(defn gaussian-noise-factor
  "Returns gaussian noise of mean 0, std dev 1."
  []
  (* (Math/sqrt (* -2.0 (Math/log (lrand))))
     (Math/cos (* 2.0 Math/PI (lrand)))))

(defn perturb-with-gaussian-noise 
  "Returns n perturbed with std dev sd."
  [sd n]
  (+' n (* sd (gaussian-noise-factor))))

(defn perturb-code-with-gaussian-noise
  "Returns code with each float literal perturbed with std dev sd and perturbation probability
   num-perturb-probability."
  [code per-num-perturb-probability sd]
  (postwalklist (fn [item]
                  (if (and (float? item)
                           (< (lrand) per-num-perturb-probability))
                    (perturb-with-gaussian-noise sd item)
                    item))
                code))

(defn gaussian-mutate 
  "Returns a gaussian-mutated version of the given individual."
  [ind per-num-perturb-probability sd]
  (make-individual 
    :program (perturb-code-with-gaussian-noise (:program ind) per-num-perturb-probability sd)
    :history (:history ind)
    :ancestors (if maintain-ancestors
                 (cons (:program ind) (:ancestors ind))
                 (:ancestors ind))))

(defn crossover 
  "Returns a copy of parent1 with a random subprogram replaced with a random 
   subprogram of parent2."
  [parent1 parent2 max-points]
  (let [new-program (insert-code-at-point 
                      (:program parent1) 
                      (select-node-index (:program parent1))
                      (code-at-point (:program parent2)
                                     (select-node-index (:program parent2))))]
    (if (> (count-points new-program) max-points)
      parent1
      (make-individual :program new-program :history (:history parent1)
                       :ancestors (if maintain-ancestors
                                    (cons (:program parent1) (:ancestors parent1))
                                    (:ancestors parent1))))))

(defn evaluate-individual
  "Returns the given individual with errors, total-errors, and hah-errors,
   computing them if necessary."
  [i error-function rand-gen]
  (binding [*thread-local-random-generator* rand-gen]
    (let [p (:program i)
          e (if (and (seq? (:errors i)) @global-reuse-errors)
              (:errors i)
              (error-function p))
          te (if (and (number? (:total-error i)) @global-reuse-errors)
               (:total-error i)
               (keep-number-reasonable (compute-total-error e)))
          he (compute-hah-error e)]
      (make-individual :program p :errors e :total-error te :hah-error he
                       :history (if maintain-histories (cons te (:history i)) (:history i))
                       :ancestors (:ancestors i)))))

(defn breed
  "Replaces the state of the given agent with an individual bred from the given population (pop), 
   using the given parameters."
  [agt location rand-gen pop error-function population-size max-points atom-generators 
   mutation-probability  mutation-max-points crossover-probability simplification-probability 
   tournament-size reproduction-simplifications trivial-geography-radius
   gaussian-mutation-probability gaussian-mutation-per-number-mutation-probability 
   gaussian-mutation-standard-deviation]
  (binding [*thread-local-random-generator* rand-gen]
    (let [n (lrand)]
      (cond 
        ;; mutation
        (< n mutation-probability)
        (mutate (select pop tournament-size trivial-geography-radius location) 
                mutation-max-points max-points atom-generators)
        ;; crossover
        (< n (+ mutation-probability crossover-probability))
        (let [first-parent (select pop tournament-size trivial-geography-radius location)
              second-parent (select pop tournament-size trivial-geography-radius location)]
          (crossover first-parent second-parent max-points))
        ;; simplification
        (< n (+ mutation-probability crossover-probability simplification-probability))
        (auto-simplify (select pop tournament-size trivial-geography-radius location)
                       error-function reproduction-simplifications false 1000)
        ;; gaussian mutation
        (< n (+ mutation-probability crossover-probability simplification-probability 
                gaussian-mutation-probability))
        (gaussian-mutate (select pop tournament-size trivial-geography-radius location) 
                         gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation)
        ;; replication
        true 
        (select pop tournament-size trivial-geography-radius location)))))

(defmacro print-params
  [params]
  (cons 'do (doall (map #(list 'println (str %) "=" %) params))))

(defn decimate
  "Returns the subset of the provided population remaining after sufficiently many
   elimination tournaments to reach the provided target-size."
  [population target-size tournament-size radius]
  (let [popsize (count population)]
    (if (<= popsize target-size)
      population
      (recur (let [tournament-index-set 
                   (let [first-location (lrand-int popsize)]
                     (cons first-location
                           (doall
                             (for [_ (range (dec tournament-size))]
                               (if (zero? radius)
                                 (lrand-int popsize)
                                 (mod (+ first-location (- (lrand-int (+ 1 (* radius 2))) radius))
                                      popsize))))))
                   victim-index
                   (reduce (fn [i1 i2] 
                             (if (> (:total-error (nth population i1))
                                    (:total-error (nth population i2)))
                               i1 
                               i2))
                           tournament-index-set)]
               (vec (concat (subvec population 0 victim-index)
                            (subvec population (inc victim-index)))))
             target-size tournament-size radius))))

(defn scaled-errors
  "A utility function for use in error functions, to implement error-scaling as described
   by Maarten Keijzer in Scaled Symbolic Regression, in Genetic Programming and Evolvable
   Machines 5(3), pp. 259-269, September 2004. This returns a sequence of scaled errors given
   a sequence of outputs, a sequence of targets, and a penalty. If there are any non-numeric
   items in the outputs, or if all of the outputs are the same, then all of the scaled errors
   will be equal to the penalty -- note that this means that you cannot use this method
   to solve a problem for which all targets are the same. An optional fourth argument,
   if true, causes the scaling slope and intercept to be printed; this is necessary for
   unscaling outputs of an evolved solution -- see examples/scaled_sextic.clj for an
   example."
  ([outputs targets penalty]
    (scaled-errors outputs targets penalty false))
  ([outputs targets penalty print-slope-and-intercept]
    (if (or (some #(not (number? %)) outputs) (apply = outputs))
      (doall (repeat (count outputs) penalty))
      (let [average-output (/ (reduce +' outputs) (count outputs))
            average-target (/ (reduce +' targets) (count targets))
            slope (/ 
                    (reduce +' 
                            (map (fn [target output]
                                   (*' (-' target average-target) 
                                       (-' output average-output)))
                                 targets
                                 outputs))
                    (reduce +'
                            (map (fn [output] (math/expt (-' output average-output) 2))
                                 outputs)))
            intercept (-' average-target  (*' slope average-output))]
        (when print-slope-and-intercept 
          (println "slope " slope ", intercept " intercept))
        (doall (map (fn [target output]
                      (math/expt (float (-' target (+' intercept (*' slope output)))) 2))
                    targets
                    outputs))))))

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

(defn pushgp
  "The top-level routine of pushgp."
  [& {:keys [error-function error-threshold population-size max-points atom-generators max-generations
             max-mutations mutation-probability mutation-max-points crossover-probability 
             simplification-probability tournament-size report-simplifications final-report-simplifications
             reproduction-simplifications trivial-geography-radius decimation-ratio decimation-tournament-size
             evalpush-limit evalpush-time-limit node-selection-method node-selection-leaf-probability
             node-selection-tournament-size pop-when-tagging gaussian-mutation-probability 
             gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation
             reuse-errors problem-specific-report use-single-thread random-seed 
             use-historically-assessed-hardness use-lexicase-selection]
      :or {error-function (fn [p] '(0)) ;; pgm -> list of errors (1 per case)
           error-threshold 0
           population-size 1000
           max-points 50
           atom-generators (concat @registered-instructions
                                   (list 
                                     (fn [] (lrand-int 100))
                                     (fn [] (lrand))))
           max-generations 1001
           mutation-probability 0.4
           mutation-max-points 20
           crossover-probability 0.4
           simplification-probability 0.1
           tournament-size 7
           report-simplifications 100
           final-report-simplifications 1000
           reproduction-simplifications 1
           trivial-geography-radius 0
           decimation-ratio 1
           decimation-tournament-size 2
           evalpush-limit 150
           evalpush-time-limit 0
           node-selection-method :unbiased
           node-selection-leaf-probability 0.1
           node-selection-tournament-size 2
           pop-when-tagging true
           gaussian-mutation-probability 0.0
           gaussian-mutation-per-number-mutation-probability 0.5
           gaussian-mutation-standard-deviation 0.1
           reuse-errors true
           problem-specific-report default-problem-specific-report
           use-single-thread false
           random-seed (System/nanoTime)   
           use-historically-assessed-hardness false    
           use-lexicase-selection false    
           }}]
  (binding [*thread-local-random-generator* (java.util.Random. random-seed)]
    ;; set globals from parameters
    (reset! global-atom-generators atom-generators)
    (reset! global-max-points-in-program max-points)
    (reset! global-evalpush-limit evalpush-limit)
    (reset! global-evalpush-time-limit evalpush-time-limit)
    (reset! global-node-selection-method node-selection-method)
    (reset! global-node-selection-leaf-probability node-selection-leaf-probability)
    (reset! global-node-selection-tournament-size node-selection-tournament-size)
    (reset! global-pop-when-tagging pop-when-tagging)
    (reset! global-reuse-errors reuse-errors)
    (reset! global-use-historically-assessed-hardness use-historically-assessed-hardness)
    (reset! global-use-lexicase-selection use-lexicase-selection)
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
             (flush)))
    (print-params 
      (error-function error-threshold population-size max-points atom-generators max-generations 
                      mutation-probability mutation-max-points crossover-probability
                      simplification-probability gaussian-mutation-probability 
                      gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation
                      tournament-size report-simplifications final-report-simplifications
                      trivial-geography-radius decimation-ratio decimation-tournament-size evalpush-limit
                      evalpush-time-limit node-selection-method node-selection-tournament-size
                      node-selection-leaf-probability pop-when-tagging reuse-errors
                      use-single-thread random-seed use-historically-assessed-hardness
                      use-lexicase-selection
                      ))
    (printf "\nGenerating initial population...\n") (flush)
    (let [pop-agents (vec (doall (for [_ (range population-size)] 
                                   ((if use-single-thread atom agent)
                                        (make-individual 
                                          :program (random-code max-points atom-generators))
                                        :error-handler (fn [agnt except] (println except))))))
          child-agents (vec (doall (for [_ (range population-size)]
                                     ((if use-single-thread atom agent)
                                          (make-individual)
                                          :error-handler (fn [agnt except] (println except))))))
          rand-gens (vec (doall (for [k (range population-size)]
                                  (java.util.Random. (+ random-seed (inc k))))))]
      (loop [generation 0]
        (printf "\n\n-----\nProcessing generation: %s\nComputing errors..." generation) (flush)
        (dorun (map #((if use-single-thread swap! send) % evaluate-individual error-function %2) pop-agents rand-gens))
        (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE ; might this need a dorun?
        (printf "\nDone computing errors.") (flush)
        ;; calculate solution rates if necessary for historically-assessed hardness
        (when (and use-historically-assessed-hardness
                   (not use-lexicase-selection))
          (reset! solution-rates
                  (let [error-seqs (map :errors (map deref pop-agents))
                        num-cases (count (first error-seqs))]
                    (doall (for [i (range num-cases)]
                             (/ (count (filter #(<= % error-threshold) (map #(nth % i) error-seqs)))
                                population-size)))))
          (printf "\nSolution rates: ")
          (println (doall (map float @solution-rates))))
        ;; report and check for success
        (let [best (report (vec (doall (map deref pop-agents))) generation error-function 
                           report-simplifications problem-specific-report)]
          (if (<= (:total-error best) error-threshold)
            (do (printf "\n\nSUCCESS at generation %s\nSuccessful program: %s\nErrors: %s\nTotal error: %s\nHistory: %s\nSize: %s\n\n"
                        generation (not-lazy (:program best)) (not-lazy (:errors best)) (:total-error best) 
                        (not-lazy (:history best)) (count-points (:program best)))
                (when print-ancestors-of-solution
                  (printf "\nAncestors of solution:\n")
                  (println (:ancestors best)))
                (auto-simplify best error-function final-report-simplifications true 500))
            (do (if (>= generation max-generations)
                  (printf "\nFAILURE\n")
                  (do (printf "\nProducing offspring...") (flush)
                      (let [pop (decimate (vec (doall (map deref pop-agents))) 
                                          (int (* decimation-ratio population-size))
                                          decimation-tournament-size 
                                          trivial-geography-radius)]
                        (dotimes [i population-size]
                          ((if use-single-thread swap! send)
                               (nth child-agents i) 
                               breed i (nth rand-gens i) pop error-function population-size max-points atom-generators 
                               mutation-probability mutation-max-points crossover-probability 
                               simplification-probability tournament-size reproduction-simplifications 
                               trivial-geography-radius gaussian-mutation-probability 
                               gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation)))
                      (when-not use-single-thread (apply await child-agents)) ;; SYNCHRONIZE
                      (printf "\nInstalling next generation...") (flush)
                      (dotimes [i population-size]
                        ((if use-single-thread swap! send)
                             (nth pop-agents i) (fn [av] (deref (nth child-agents i)))))
                      (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
                      (recur (inc generation)))))))))))

(defn pushgp-map
  "Calls pushgp with the args in argmap."
  [argmap]
  (apply pushgp (apply concat argmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stress test

(defn stress-test
  "Performs a stress test of the registered instructions by generating and running n
   random programs. For more thorough testing and debugging of Push instructions you many
   want to un-comment code in execute-instruction that will allow you to look at recently
   executed instructions and the most recent state after an error. That code burns memory,
   however, so it is normally commented out. You might also want to comment out the handling
   of nil values in execute-instruction, do see if any instructions are introducing nils."
  [n]
  (let [completely-random-program
        (fn []
          (random-code 100 (concat @registered-instructions
                                   (list (fn [] (lrand-int 100))
                                         (fn [] (lrand))))))]
    (loop [i 0 p (completely-random-program)]
      (if (>= i n)
        (println :no-errors-found-in-stress-test)
        (let [result (run-push p (make-push-state) false)]
          (if result
            (recur (inc i) (completely-random-program))
            (println p)))))))

;(stress-test 10000)

(defn -main 
  "A main function for clojush, which assumes that the first/only argument is the name
   of a problem file that contains a top level call. Exits after completion of the call.
   This allows one to run an example with a call from the OS shell prompt like:
       lein run examples.simple-regression"
  [& args]
  (use (symbol (first args)))
  (System/exit 0))
