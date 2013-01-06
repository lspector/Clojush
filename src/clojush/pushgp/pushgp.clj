(ns clojush.pushgp.pushgp
  (:use [clojush.globals]
        [clojush.util]
        [clojush.pushstate]
        [clojush.random]
        [clojush.instructions.boolean]
        [clojush.instructions.code]
        [clojush.instructions.common]
        [clojush.instructions.numbers]
        [clojush.instructions.random-instructions]
        [clojush.instructions.string]
        [clojush.instructions.tag]
        [clojush.instructions.zip]
        [clojush.instructions.return]
        [clojush.individual]
        [clojush.evaluate]
        [clojush.pushgp.breed]
        [clojush.pushgp.parent-selection]
        [clojush.pushgp.report]
        [clojush.experimental.decimation]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushgp

(defn pushgp
  "The top-level routine of pushgp."
  [& {:keys [error-function error-threshold population-size max-points
             max-points-in-initial-program atom-generators max-generations 
             max-mutations mutation-probability mutation-max-points 
             crossover-probability simplification-probability
             tournament-size report-simplifications final-report-simplifications
             reproduction-simplifications trivial-geography-radius decimation-ratio
             decimation-tournament-size evalpush-limit evalpush-time-limit
             node-selection-method node-selection-leaf-probability
             node-selection-tournament-size pop-when-tagging
             gaussian-mutation-probability
             gaussian-mutation-per-number-mutation-probability
             gaussian-mutation-standard-deviation reuse-errors
             problem-specific-report use-single-thread random-seed
             use-historically-assessed-hardness use-lexicase-selection
             use-rmse print-csv-logs print-json-logs csv-log-filename
             json-log-filename log-fitnesses-for-all-cases
             json-log-program-strings
             boolean-gsxover-probability
             boolean-gsxover-new-code-max-points
             deletion-mutation-probability
             parentheses-addition-mutation-probability
             tagging-mutation-probability
             tag-branch-mutation-probability
             tag-branch-mutation-type-instruction-pairs
             parent-reversion-probability
             tag-limit
             ]
      :or {error-function (fn [p] '(0)) ;; pgm -> list of errors (1 per case)
           error-threshold 0
           population-size 1000
           max-points 50
           max-points-in-initial-program 50
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
           boolean-gsxover-probability 0.0
           boolean-gsxover-new-code-max-points 20
           deletion-mutation-probability 0.0
           parentheses-addition-mutation-probability 0.0
           tagging-mutation-probability 0.0
           tag-branch-mutation-probability 0.0
           tag-branch-mutation-type-instruction-pairs []
           reuse-errors true
           problem-specific-report default-problem-specific-report
           print-csv-logs false
           print-json-logs false
           csv-log-filename "log.csv"
           json-log-filename "log.json"
           log-fitnesses-for-all-cases false
           json-log-program-strings false
           use-single-thread false
           random-seed (System/nanoTime)
           use-historically-assessed-hardness false
           use-lexicase-selection false
           use-rmse false
           parent-reversion-probability 0.0
           tag-limit 10000
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
    (reset! global-use-rmse use-rmse)
    (reset! global-tag-limit tag-limit)
    (initial-report) ;; Print the inital report
    (print-params 
      (atom-generators print-csv-logs csv-log-filename print-json-logs
                       json-log-filename log-fitnesses-for-all-cases
                       json-log-program-strings error-function error-threshold
                       population-size max-generations max-points
                       max-points-in-initial-program
                       mutation-probability mutation-max-points crossover-probability
                       simplification-probability gaussian-mutation-probability 
                       gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation
                       boolean-gsxover-probability boolean-gsxover-new-code-max-points
                       deletion-mutation-probability parentheses-addition-mutation-probability
                       tagging-mutation-probability tag-branch-mutation-probability
                       tag-branch-mutation-type-instruction-pairs
                       tournament-size report-simplifications final-report-simplifications
                       reproduction-simplifications trivial-geography-radius decimation-ratio 
                       decimation-tournament-size evalpush-limit evalpush-time-limit node-selection-method 
                       node-selection-tournament-size node-selection-leaf-probability pop-when-tagging 
                       reuse-errors use-single-thread random-seed use-historically-assessed-hardness
                       use-lexicase-selection use-rmse parent-reversion-probability
                       tag-limit))
    (printf "\nGenerating initial population...\n") (flush)
    (let [pop-agents (vec (doall (for [_ (range population-size)] 
                                   ((if use-single-thread atom agent)
                                        (make-individual 
                                          :program (random-code max-points-in-initial-program atom-generators))
                                        :error-handler (fn [agnt except] (println except))))))
          child-agents (vec (doall (for [_ (range population-size)]
                                     ((if use-single-thread atom agent)
                                          (make-individual)
                                          :error-handler (fn [agnt except] (println except))))))
          rand-gens (vec (doall (for [k (range population-size)]
                                  (java.util.Random. (+ random-seed (inc k))))))]
      ;; Main loop
      (loop [generation 0]
        (printf "\n\n-----\nProcessing generation: %s\nComputing errors..." generation)
        (flush)
        (dorun (map #((if use-single-thread swap! send) % evaluate-individual error-function %2)
                    pop-agents
                    rand-gens))
        (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE ;might this need a dorun?
        (printf "\nDone computing errors.") (flush)
        ;; possible parent reversion
        (if (and (> generation 0) (> parent-reversion-probability 0))
          (let [err-fn (if @global-use-rmse :rms-error :total-error)]
            (printf "\nPerforming parent reversion...") (flush)
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
            (printf "\nDone performing parental reversion.") (flush)))
        ;; calculate solution rates if necessary for historically-assessed hardness
        (calculate-hah-solution-rates use-historically-assessed-hardness
                                      use-lexicase-selection
                                      pop-agents
                                      error-threshold
                                      population-size)
        ;; report and check for success
        (let [best (report (vec (doall (map deref pop-agents))) generation error-function 
                           report-simplifications print-csv-logs print-json-logs
                           csv-log-filename json-log-filename
                           log-fitnesses-for-all-cases json-log-program-strings
                           problem-specific-report)]
          (if (<= (:total-error best) error-threshold)
            (final-report generation best error-function final-report-simplifications)
            (do (if (>= generation max-generations)
                  (do (printf "\nFAILURE\n") (flush))
                  (do (printf "\nProducing offspring...") (flush)
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
                               [i (nth rand-gens i) pop error-function max-points atom-generators 
                                mutation-probability mutation-max-points crossover-probability 
                                simplification-probability tournament-size reproduction-simplifications 
                                trivial-geography-radius gaussian-mutation-probability 
                                gaussian-mutation-per-number-mutation-probability gaussian-mutation-standard-deviation
                                boolean-gsxover-probability boolean-gsxover-new-code-max-points
                                deletion-mutation-probability parentheses-addition-mutation-probability
                                tagging-mutation-probability tag-branch-mutation-probability
                                tag-branch-mutation-type-instruction-pairs])))
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
