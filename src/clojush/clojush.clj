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
    [clojush.pushstate]
    [clojush.interpreter]
    [clojush.util]
    [clojush.random]
    [clojush.instructions.boolean]
    [clojush.instructions.code]
    [clojush.instructions.common]
    [clojush.instructions.numbers]
    [clojush.instructions.random]
    [clojush.instructions.string]
    [clojush.instructions.tag]
    [clojush.instructions.zip]
    [clojush.pushgp.individual]
    [clojush.pushgp.evaluate]
    [clojush.pushgp.parent_selection]
    [clojush.pushgp.node_selection]
    [clojush.pushgp.simplification]
    [clojush.pushgp.report]
    [clojush.experimental.tagged_code_macros]))

(import java.lang.Math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; small things during initialization

;; backtrace abbreviation, to ease debugging
(defn bt []
  (.printStackTrace *e))

;; set default value for atom-generators
(reset! global-atom-generators 
        (concat @registered-instructions
                (list 
                  (fn [] (lrand-int 100))
                  (fn [] (lrand)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushgp


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
            (final-report generation best error-function final-report-simplifications)
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
